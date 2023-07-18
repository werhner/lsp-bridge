import subprocess
import re
import os
from core.utils import eval_in_emacs

DEFAULT_FILTER_CMD = '(not (or (and $extras ((string->regexp "(^|,) ?(anonymous|reference)(,|$)" :case-fold false) $extras)) (or (and $extras ((string->regexp "(^|,) ?(inputFile)(,|$)" :case-fold false) $extras)) (and $kind ((string->regexp "^(file|F)$" :case-fold false) $kind))) false))'

DEFAULT_SORTER_CMD = '(<or> (if (and $name &name) (<> (length $name) (length &name)) 0) (if (and $name &name) (<> $name &name) 0))'

class Ctags:    
    def run_cmd_in_path(self, cmd, filename, in_shell=False):
        if os.path.isfile(filename):
            cwd = os.path.dirname(filename)
        else:
            cwd = filename

        #print(cmd)
        result = subprocess.run(cmd, cwd=cwd, shell= in_shell, capture_output=True, text=True)

        # Get the output as a string
        output = ""
        status = result.returncode
        if status != 0:
            print("error")
            print(result.stderr)
        else:
            output = result.stdout
        
        lines = output.split('\n')
        lines = [l for l in lines if l != '']

        return lines

    def get_emacs_path(self, filename):
        emacs_path = filename.rsplit(":", 1)

        if len(emacs_path) == 2:
            remote_method = emacs_path[0]
            true_filepath = emacs_path[1]
        else:
            remote_method = ""
            true_filepath = emacs_path[0]

        return (remote_method, true_filepath)

    def parse_tag_line(self, line):
        if line == '':
            return None
    
        tag = {}
        entry = line.rsplit(';"', 1)
    
        normal_field = entry[0].split('\t', 2)
        # print(line)
        tag['tagname'] = normal_field[0]
        tag['tagfile'] = normal_field[1]
        tag['tagaddress'] = normal_field[2]
    
        if len(entry) == 2:
            for field in entry[1].split('\t')[1:]:
                field_entry = field.split(':', 1)
                # print(field_entry)
                if len(field_entry) == 2:
                    tag.update(dict([field_entry]))
                else:
                    tag.update({"kind": field_entry})
                
        return tag

    def make_tag_annotation(self, tag):
        kind = tag.get("kind", "")
        typeref = tag.get("typeref", "")
        scope = tag.get("scope", "")
        extras = tag.get("extras", "")
        reference = "<R>" if "reference" in extras else ""
    
        typeref = typeref.removeprefix("typename:")
        typeref = re.sub(r'(^|:)(__anon[^:]+)(:|$)', r'\1__anon\3', typeref)
        scope = re.sub(r'(^|:)(__anon[^:]+)(:|$)', r'\1__anon\3', scope)
    
        annotation = (reference, 
                      kind, 
                      "/" if (kind != "" and typeref != "") else "",
                      typeref,
                      "@" if (scope != "") else "",
                      scope)

        return ''.join(annotation)

    def make_ctags_acm_candidate(self, tag: dict):
        candidate = {}
        tagname = tag["tagname"]
    
        kind = tag.get("kind", "NOKIND")
        signature = tag.get("signature", "NOSIGN")
        annotation = self.make_tag_annotation(tag)
    
        candidate["key"] = tagname
        candidate["icon"] = kind
        candidate["label"] = tagname
        candidate["display-label"] = tagname
        candidate["annotation"] = annotation
        candidate["backend"] = "ctags"

        return candidate

    def make_ctags_jump(self, tag: dict, rootdir):
        candidate = {}
        tagname = tag["tagname"]

        path = tag.get("tagfile", "")
        line = int(tag.get("line", "1"))
        pattern = tag.get("tagaddress", "")
        annotation = self.make_tag_annotation(tag)
        
        ext_abspath = os.path.join(rootdir, path)

        candidate["ext-abspath"] = ext_abspath
        candidate["line"] = line
        candidate["str"] = pattern
        candidate["annotation"] = annotation

        return candidate
        
    def make_complete(self, symbol, filename):
        cmd = self.readtags_get_cmd("", symbol, "prefix", False, DEFAULT_FILTER_CMD, DEFAULT_SORTER_CMD, "")
        
        lines = self.run_cmd_in_path(cmd, filename)
    
        tags = map(self.parse_tag_line, lines)
        candidates = map(self.make_ctags_acm_candidate, tags)
        
        candidates = list(candidates)
        # return list(candidates)

    def find_definition(self, symbol, filename):
        (remote_method, filename) = self.get_emacs_path(filename)
        
        cmd = self.readtags_get_cmd("", symbol, "exact", False, "", "", "")
        
        rootdir = self.run_cmd_in_path("readtags -D | grep 'TAG_PROC_CWD' | cut -f2", filename, in_shell=True)
        if rootdir:
            rootdir = rootdir[0]
        else:
            rootdir = ""

        #print(rootdir)
        lines = self.run_cmd_in_path(cmd, filename)
        #print(lines)
        tags = map(self.parse_tag_line, lines)
        candidates = map(lambda tag: self.make_ctags_jump(tag, rootdir), tags)

        candidates = list(candidates)
        #print(candidates)
        eval_in_emacs("lsp-bridge-xref-callback", candidates)

    def readtags_get_cmd(self, tagsfile, name, match, case_fold, filter, sorter, action):
        extras = ("-Ene" +
                  ("" if match == "exact" else "p") +
                  ("i" if case_fold else ""))
        cmd = []
        
        # program name        
        cmd.append("readtags")

        # read from this tags file
        if tagsfile:
            cmd.append("-t")
            cmd.append(tagsfile)

        # filter expresion
        if filter:
            cmd.append("-Q")
            cmd.append(filter)
        if sorter:
            cmd.append("-S")
            cmd.append(sorter)

        # extras arguments
        cmd.append(extras)

        # action
        #cmd.append(action if action else "-l" if not name else ("- " + name))
        if action:
            cmd.append(action)
        else:
            if not name:
                cmd.append("-l")
            else:
                cmd.append("-")
                cmd.append(name)

        #cmd = " ".join(cmd)
        return cmd
        
# (citre-tags-completion-default-filter "DEFUN")
