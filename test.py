# "(#(\"DEFUN\" 0 5 (citre-kind macro citre-signature \"(lname,fnname,sname,minargs,maxargs,intspec,doc)\" citre-annotation #(\" (macro)\" 0 2 (face citre-tag-annotation-face) 2 7 (face citre-tag-annotation-face) 7 8 (face citre-tag-annotation-face)) face font-lock-keyword-face))

# readtags -t /Users/bytedance/code/emacs/tags -Q '(not (or (and $extras ((string->regexp "(^|,) ?(anonymous|reference)(,|$)" :case-fold false) $extras)) (or (and $extras ((string->regexp "(^|,) ?(inputFile)(,|$)" :case-fold false) $extras)) (and $kind ((string->regexp "^(file|F)$" :case-fold false) $kind))) false))' -S '(<or> (if (and $name &name) (<> (length $name) (length &name)) 0) (if (and $name &name) (<> $name &name) 0))' -Enep - DEFUN

import core.ctags

ctags = core.ctags.Ctags()

#ctags.make_complete("stretch", "/Users/werhner/code/linux/tags")
ctags.find_definition("stretch", "/Users/werhner/code/linux/tags")

import os
#print(os.path.join("/Users/werhner/code/emacs/tags", "../stretch"))
#print(os.path.join("/Users/werhner/code/emacs/tags", "/code/path"))
#print(os.path.join("Users/werhner/code/emacs/tags", "/code/path"))
#print(os.path.join("Users/werhner/code/emacs/tags", "code/path"))

def get_emacs_path(filepath):
    emacs_path = filepath.rsplit(":", 1)
    print(emacs_path)
    if len(emacs_path) == 2:
        remote_method = emacs_path[0]
        true_filepath = emacs_path[1]
    else:
        remote_method = ""
        true_filepath = emacs_path[0]

    return (remote_method, true_filepath)

get_emacs_path("/ssh:dev:/e/")
get_emacs_path("/emacs/code")
        
# (citre-tags-get-symbol "/Users/werhner/code/emacs/tags")

# (message "%s" (citre-tags-completion-default-filter "DEFUN"))

# (lsp-bridge-ctags-find-def "stretch" "/Users/werhner/code/linux/tags")
