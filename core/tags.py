#! /usr/bin/env python
# -*- coding: utf-8 -*-

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import subprocess
import threading
import os

from core.utils import *

class tags():
    def __init__(self) -> None:
        self.current_cursor_offset = 0
        self.lock = threading.RLock()

    def run_cmd_in_path(self, cmd, filename, in_shell=False):
        if os.path.isfile(filename):
            cwd = os.path.dirname(filename)
        else:
            cwd = filename

        result = subprocess.run(cmd, cwd=cwd, shell=in_shell, capture_output=True, text=True)

        # Get the output as a string
        output = ""
        status = result.returncode
        if status != 0:
            logger.error(result.stderr)
        else:
            output = result.stdout

        lines = output.split('\n')
        lines = [l for l in lines if l != '']

        return lines

    def locate_dominating_file(self, file_or_dir, filename):
        current_dir = os.path.abspath(file_or_dir)
        
        while True:
            if os.path.exists(os.path.join(current_dir, filename)):
                return os.path.join(current_dir, filename)

            if current_dir == os.path.dirname(current_dir):
                break

            current_dir = os.path.dirname(current_dir)
            
        return None

    def update_cursor(self, cursor_offset):
        self.lock.acquire()
        try:
            self.current_cursor_offset = cursor_offset
        finally:
            self.lock.release()

    def dispatch(self, backend, candidates, cursor_offset):
        self.lock.acquire()
        try:
            if self.current_cursor_offset == cursor_offset:
                eval_in_emacs("lsp-bridge-search-backend--record-items", backend, candidates)
        finally:
            self.lock.release()
