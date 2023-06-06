;;; tramp-bridge.el --- Tramp access functions for lsp-bridge

;;; Commentary:

;; The file name handler

;;; Code:

(require 'tramp)

;;;###tramp-autoload
;(tramp--with-startup
 (add-to-list 'tramp-methods
              '("bridge"
                (tramp-remote-shell         "/bin/sh")))
;)

;; New handlers should be added here.
;;;###tramp-autoload
(defconst tramp-bridge-file-name-handler-alist
  '((abbreviate-file-name . tramp-handle-abbreviate-file-name)
    (access-file . tramp-handle-access-file)
    (add-name-to-file . tramp-bridge-handle-add-name-to-file)
    ;; `byte-compiler-base-file-name' performed by default handler.
    (copy-directory . tramp-bridge-handle-copy-directory)
    (copy-file . tramp-bridge-handle-copy-file)
    (delete-directory . tramp-bridge-handle-delete-directory)
    (delete-file . tramp-bridge-handle-delete-file)
    ;; `diff-latest-backup-file' performed by default handler.
    (directory-file-name . tramp-handle-directory-file-name)
    (directory-files . tramp-handle-directory-files)
    (directory-files-and-attributes
     . tramp-bridge-handle-directory-files-and-attributes)
    ;; Starting with Emacs 29.1, `dired-compress-file' performed by
    ;; default handler.
    (dired-compress-file . tramp-bridge-handle-dired-compress-file)
    (dired-uncache . tramp-handle-dired-uncache)
    (exec-path . tramp-bridge-handle-exec-path)
    ;(expand-file-name . tramp-bridge-handle-expand-file-name)
    (expand-file-name . tramp-handle-expand-file-name)
    (file-accessible-directory-p . tramp-handle-file-accessible-directory-p)
    (file-acl . tramp-bridge-handle-file-acl)
    (file-attributes . tramp-bridge-handle-file-attributes)
    (file-directory-p . tramp-bridge-handle-file-directory-p)
    (file-equal-p . tramp-handle-file-equal-p)
    (file-executable-p . tramp-bridge-handle-file-executable-p)
    (file-exists-p . tramp-bridge-handle-file-exists-p)
    (file-in-directory-p . tramp-handle-file-in-directory-p)
    (file-local-copy . tramp-bridge-handle-file-local-copy)
    (file-locked-p . tramp-handle-file-locked-p)
    (file-modes . tramp-handle-file-modes)
    (file-name-all-completions . tramp-bridge-handle-file-name-all-completions)
    (file-name-as-directory . tramp-handle-file-name-as-directory)
    (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p)
    (file-name-completion . tramp-handle-file-name-completion)
    (file-name-directory . tramp-handle-file-name-directory)
    (file-name-nondirectory . tramp-handle-file-name-nondirectory)
    ;; `file-name-sans-versions' performed by default handler.
    (file-newer-than-file-p . tramp-handle-file-newer-than-file-p)
    (file-notify-add-watch . tramp-bridge-handle-file-notify-add-watch)
    (file-notify-rm-watch . tramp-handle-file-notify-rm-watch)
    (file-notify-valid-p . tramp-handle-file-notify-valid-p)
    (file-ownership-preserved-p . tramp-bridge-handle-file-ownership-preserved-p)
    (file-readable-p . tramp-bridge-handle-file-readable-p)
    (file-regular-p . tramp-handle-file-regular-p)
    (file-remote-p . tramp-handle-file-remote-p)
    (file-selinux-context . tramp-bridge-handle-file-selinux-context)
    (file-symlink-p . tramp-handle-file-symlink-p)
    (file-system-info . tramp-bridge-handle-file-system-info)
    (file-truename . tramp-bridge-handle-file-truename)
    (file-user-uid . tramp-handle-file-user-uid)
    (file-writable-p . tramp-bridge-handle-file-writable-p)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    ;; `get-file-buffer' performed by default handler.
    (insert-directory . tramp-bridge-handle-insert-directory)
    (insert-file-contents . tramp-handle-insert-file-contents)
    (list-system-processes . tramp-handle-list-system-processes)
    (load . tramp-handle-load)
    (lock-file . tramp-handle-lock-file)
    (make-auto-save-file-name . tramp-handle-make-auto-save-file-name)
    (make-directory . tramp-bridge-handle-make-directory)
    ;; `make-directory-internal' performed by default handler.
    (make-lock-file-name . tramp-handle-make-lock-file-name)
    (make-nearby-temp-file . tramp-handle-make-nearby-temp-file)
    (make-process . tramp-bridge-handle-make-process)
    (make-symbolic-link . tramp-bridge-handle-make-symbolic-link)
    (memory-info . tramp-handle-memory-info)
    (process-attributes . tramp-handle-process-attributes)
    (process-file . tramp-bridge-handle-process-file)
    (rename-file . tramp-bridge-handle-rename-file)
    (set-file-acl . tramp-bridge-handle-set-file-acl)
    (set-file-modes . tramp-bridge-handle-set-file-modes)
    (set-file-selinux-context . tramp-bridge-handle-set-file-selinux-context)
    (set-file-times . tramp-bridge-handle-set-file-times)
    (set-visited-file-modtime . tramp-bridge-handle-set-visited-file-modtime)
    (shell-command . tramp-handle-shell-command)
    (start-file-process . tramp-handle-start-file-process)
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)
    (temporary-file-directory . tramp-handle-temporary-file-directory)
    (tramp-get-home-directory . tramp-bridge-handle-get-home-directory)
    (tramp-get-remote-gid . tramp-bridge-handle-get-remote-gid)
    (tramp-get-remote-groups . tramp-bridge-handle-get-remote-groups)
    (tramp-get-remote-uid . tramp-bridge-handle-get-remote-uid)
    (tramp-set-file-uid-gid . tramp-bridge-handle-set-file-uid-gid)
    (unhandled-file-name-directory . ignore)
    (unlock-file . tramp-handle-unlock-file)
    (vc-registered . tramp-bridge-handle-vc-registered)
    (verify-visited-file-modtime . tramp-bridge-handle-verify-visited-file-modtime)
    (write-region . tramp-bridge-handle-write-region))
  "Alist of handler functions.
Operations not mentioned here will be handled by the normal Emacs functions.")

;;;###tramp-autoload
(defun tramp-bridge-file-name-handler (operation &rest args)
  "Invoke remote-shell Tramp file name handler.
Fall back to normal file name handler if no Tramp handler exists."
  (if-let ((fn (assoc operation tramp-bridge-file-name-handler-alist)))
      (save-match-data (apply (cdr fn) args))
    (tramp-run-real-handler operation args)))

;;;###tramp-autoload
(defun tramp-bridge-file-name-handler-p (vec)
  "Whether VEC uses a method from `tramp-sh-file-name-handler'."
  (and (assoc (tramp-file-name-method vec) tramp-methods)
       (eq (tramp-find-foreign-file-name-handler vec)
	   'tramp-bridge-file-name-handler)))

;; It must be a `defsubst' in order to push the whole code into
;; tramp-loaddefs.el.  Otherwise, there would be recursive autoloading.
;;;###tramp-autoload
(defsubst tramp-bridge-file-name-p (vec-or-filename)
  "Check if it's a VEC-OR-FILENAME for bridge."
  (when-let* ((vec (tramp-ensure-dissected-file-name vec-or-filename)))
    (string= (tramp-file-name-method vec) "bridge")))

;;;###tramp-autoload
;(tramp--with-startup
 (add-to-list 'tramp-foreign-file-name-handler-alist
	      (cons #'tramp-bridge-file-name-p #'tramp-bridge-file-name-handler))
;)

(provide 'tramp-bridge)
;;; tramp-bridge.el ends here
