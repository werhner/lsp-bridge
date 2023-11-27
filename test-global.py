from core.gtags import Gtags
import os

gtags = Gtags()
gtags.find_definition("CMCOST", "/home/werhner/code/emacs/src/cm.c")
gtags.find_definition("CMCOST", "/home/werhner/code/emacs/doc/emacs")

'''
(lsp-bridge-gtags-find-def "CMCOST" "/home/werhner/code/emacs/doc/emacs")
(lsp-bridge-gtags-find-ref "CMCOST" "/home/werhner/code/emacs/doc/emacs")

(lsp-bridge-ctags-find-def "cmcheckmagic" "/home/werhner/code/emacs/doc/emacs")
(lsp-bridge-ctags-find-def "CMCOST" "/home/werhner/code/emacs/doc/emacs")

(lsp-bridge-gtags-find-ref "buffer_p" "/home/werhner/code/emacs/doc/emacs")

(propertize (ansi-color-apply "\033[95m({})\033[0m{}"))
(xterm-color-filter-strip "\033[95m({})\033[0m{}")
(propertize (concat "(" ") ") 'face 'citre-tag-annotation-face)
'''
