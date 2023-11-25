from core.gtags import Gtags
import os

gtags = Gtags()
gtags.find_definition("CMCOST", "/home/werhner/code/emacs/src/cm.c")
gtags.find_definition("CMCOST", "/home/werhner/code/emacs/doc/emacs")

'''
(lsp-bridge-gtags-find-def "CMCOST" "/home/werhner/code/emacs/doc/emacs")
(lsp-bridge-gtags-find-ref "CMCOST" "/home/werhner/code/emacs/doc/emacs")
'''
