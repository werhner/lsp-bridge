# "(#(\"DEFUN\" 0 5 (citre-kind macro citre-signature \"(lname,fnname,sname,minargs,maxargs,intspec,doc)\" citre-annotation #(\" (macro)\" 0 2 (face citre-tag-annotation-face) 2 7 (face citre-tag-annotation-face) 7 8 (face citre-tag-annotation-face)) face font-lock-keyword-face))

# readtags -t /Users/bytedance/code/emacs/tags -Q '(not (or (and $extras ((string->regexp "(^|,) ?(anonymous|reference)(,|$)" :case-fold false) $extras)) (or (and $extras ((string->regexp "(^|,) ?(inputFile)(,|$)" :case-fold false) $extras)) (and $kind ((string->regexp "^(file|F)$" :case-fold false) $kind))) false))' -S '(<or> (if (and $name &name) (<> (length $name) (length &name)) 0) (if (and $name &name) (<> $name &name) 0))' -Enep - DEFUN

import core.ctags

ctags = core.ctags.Ctags()

ctags.make_complete("stretch", "/Users/werhner/code/linux/tags")
ctags.find_definition("stretch", "/Users/werhner/code/linux/tags")

ctags.readtags_get_cmd("/Users/werhner/code/emacs/tags",
                         "",
                         "p",
                         True,
                         None,
                         None,
                         None)
import os
print(os.path.join("/Users/werhner/code/emacs/tags", "../stretch"))
print(os.path.join("/Users/werhner/code/emacs/tags", "/code/path"))
print(os.path.join("Users/werhner/code/emacs/tags", "/code/path"))
print(os.path.join("Users/werhner/code/emacs/tags", "code/path"))

# (citre-tags-get-symbol "/Users/werhner/code/emacs/tags")

# (message "%s" (citre-tags-completion-default-filter "DEFUN"))
