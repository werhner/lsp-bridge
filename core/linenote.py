import os
import re
from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler
from core.utils import eval_in_emacs, get_lsp_file_host, get_remote_connection_info
import logging
import time

# https://pravash-techie.medium.com/python-event-monitoring-with-watchdogs-86125f946da6
file_watch = {}

def parse_one_note(linenote: str):
    note = str(linenote).split('.linenote/')[1].rsplit('/', 1)[1]
    match = re.findall(r'@(\d+)#(\d+)', note)

    start_line = int(match[0][0])
    start_char = int(match[0][1])
    end_line = int(match[1][0])
    end_char = int(match[1][1])

    with open(linenote, 'r', encoding='utf-8') as file:
        first_line = file.readline().strip()

    logging.error(linenote)
    return {'range': {'start': {'line': start_line, 'character': start_char},
                      'end': {'line': end_line, 'character': end_char}},
            'message': f"{first_line}",
            'severity': 4,
            'server-name': 'linenote'
            }
    
def create_a_linenote(project: str, filename: str, start_line: int, start_char: int, end_line: int, end_char: int):
    project = os.path.expanduser(project)
    filename = os.path.expanduser(filename)
    
    linenote = os.path.relpath(filename, project)
    linenote = os.path.join(project,
                            ".linenote",
                            linenote,
                            "@{}#{}..@{}#{}.org".format(start_line, start_char, end_line, end_char))

    os.makedirs(os.path.dirname(linenote), exist_ok=True)

    if not filename in file_watch:
        open_a_file(project, filename)

    # with open(linenote, 'w') as _file:
    #     pass
    eval_in_emacs("find-file", get_remote_connection_info() + linenote)

def delete_a_linenote(project: str, filename: str, start_line: int, start_char: int, end_line: int, end_char: int):
    project = os.path.expanduser(project)
    filename = os.path.expanduser(filename)
    
    linenote = os.path.relpath(filename, project)
    linenote = os.path.join(project,
                            ".linenote",
                            linenote,
                            "@{}#{}..@{}#{}.org".format(start_line, start_char, end_line, end_char))

    os.remove(linenote)

def parse_notes(project: str, filename: str):
    project = os.path.expanduser(project)
    filename = os.path.expanduser(filename)

    file = os.path.relpath(filename, project)
    linenote_dir = os.path.join(project, ".linenote", file)

    notes = []

    for note in os.listdir(linenote_dir):
        notes.append(parse_one_note(os.path.join(linenote_dir, note)))

    eval_in_emacs("lsp-bridge-linenote--render",
                  filename,
                  get_lsp_file_host(),
                  notes,
                  len(notes))
    
    logging.error(notes)

def parse_notes_by_dir(linenote_dir: str):
    notes = []

    for note in os.listdir(linenote_dir):
        notes.append(parse_one_note(os.path.join(linenote_dir, note)))

    project = str(linenote_dir).split('.linenote/')[0]
    rel_file = str(linenote_dir).split('.linenote/')[1]

    filename = os.path.join(project, rel_file)
    eval_in_emacs("lsp-bridge-linenote--render",
                  filename,
                  get_lsp_file_host(),
                  notes,
                  len(notes))
    return notes

class EventHandler(FileSystemEventHandler):
    def on_created(self, event):
        logging.error(f"File created: {event.src_path}")

    def on_deleted(self, event):
        logging.error(f"File deleted: {event.src_path}")

    def on_modified(self, event):
        if event.is_directory:
            logging.error(parse_notes_by_dir(event.src_path))
        else:
            logging.error(parse_notes_by_dir(os.path.dirname(event.src_path)))
            logging.error(f"File modified: {event.src_path}")

def open_a_file(project: str, filename: str):
    project = os.path.expanduser(project)
    filename = os.path.expanduser(filename)

    rel_path = os.path.relpath(filename, project)
    linenote_dir = os.path.join(project,
                                ".linenote",
                                rel_path)

    if not os.path.exists(linenote_dir):
        return
    
    observer = Observer()
    observer.schedule(EventHandler(), linenote_dir)
    observer.start()
    file_watch[filename] = observer
    logging.error(f'start watching directory {linenote_dir!r}')
    
    parse_notes(project, filename)

def close_a_file(filename: str):
    filename = os.path.expanduser(filename)

    observer = file_watch[filename]
    observer.stop()
    observer.join()
    logging.error("close")
    
if __name__ == "__main__":
    open_a_file("~/.emacs.d", "~/.emacs.d/lisp/init-auto-save.el")
    create_a_linenote("~/.emacs.d", "~/.emacs.d/lisp/init-auto-save.el", 2, 3, 4, 5)

    create_a_linenote("~/.emacs.d", "~/.emacs.d/lisp/init-auto-save.el", 2, 3, 8, 5)
    
    try:
        while True:
            time.sleep(1)
    finally:
        delete_a_linenote("~/.emacs.d", "~/.emacs.d/lisp/init-auto-save.el", 2, 3, 8, 5)
        delete_a_linenote("~/.emacs.d", "~/.emacs.d/lisp/init-auto-save.el", 2, 3, 4, 5)
        close_a_file("~/.emacs.d/lisp/init-auto-save.el")

