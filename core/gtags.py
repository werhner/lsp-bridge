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
import re
import os

from core.utils import *

from core.tags import tags

class Gtags(tags):
    def make_gtags_acm_candidate(self, tagname):
        candidate = {}

        candidate["key"] = tagname
        candidate["icon"] = "gtags"
        candidate["label"] = tagname
        candidate["displayLabel"] = tagname
        candidate["annotation"] = "gtags"
        candidate["backend"] = "gtags"

        return candidate

    def make_complete(self, symbol, filename, cursor_offset):
        self.update_cursor(cursor_offset)

        if not filename:
            return

        cmd = self.global_get_cmd(symbol, "completion", True, None)
        lines = self.run_cmd_in_path(cmd, filename)
        
        candidates = map(self.make_gtags_acm_candidate, lines)

        self.dispatch("gtags", list(candidates), cursor_offset)

    def global_get_cmd(self, name, mode, case_fold, start_file):
        cmd = []

        # program name        
        cmd.append("global")
        
        if mode == "completion":
            cmd.append("--completion")
        elif mode == "definition":
            cmd.append("--definition")
        elif mode == "reference":
            cmd.append("--reference")
            cmd.append("--symbol")
        else:
            return None

        if case_fold:
            cmd.append("--ignore-case")

        if start_file:
            cmd.append("--nearness={}".format(start_file))
            
        cmd.append("--color=never")
        cmd.append("--encode-path= :")
        cmd.append("--result=grep")
        cmd.append("--literal")
        cmd.append("--")
        cmd.append(name)
        
        return cmd