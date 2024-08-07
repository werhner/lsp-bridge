;;; lsp-bridge-linenote.el --- LSP bridge  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'lsp-bridge)
(require 'flymake)

(defun lsp-bridge-linenote-open-file-hook ()
  (interactive)
  (add-hook 'flymake-diagnostic-functions #'flymake-linenote nil t)
  (add-hook 'lsp-bridge-linenote-update-hook #'flymake-start nil t)

  (let ((project (file-local-name (project-root (my-project-root))))
        (filename (file-local-name (buffer-file-name))))
    (if (lsp-bridge-is-remote-file)
        (lsp-bridge-remote-send-func-request "linenote_open_file"
                                             (list project
                                                   filename))
      (lsp-bridge-call-async "linenote_open_file"
                             project
                             filename))))

(defun lsp-bridge-linenote-close-file-hook ()
  (lsp-bridge-call-async "linenote_close_file" (file-local-name (buffer-file-name))))

(defun lsp-bridge-linenote-create ()
  (interactive)

  (let ((project (file-local-name (project-root (my-project-root))))
        (filename (file-local-name (buffer-file-name)))
        (start (lsp-bridge--point-position (if (use-region-p)
                                               (region-beginning)
                                             (line-beginning-position))))
        (end (lsp-bridge--point-position (if (use-region-p)
                                             (region-end)
                                           (point)))))
    (if (lsp-bridge-is-remote-file)
        (lsp-bridge-remote-send-func-request "linenote_create"
                                             (list project
                                                   filename
                                                   (plist-get start :line)
                                                   (plist-get start :character)
                                                   (plist-get end :line)
                                                   (plist-get end :character)))
      (lsp-bridge-call-async "linenote_create"
                             project
                             filename
                             (plist-get start :line)
                             (plist-get start :character)
                             (plist-get end :line)
                             (plist-get end :character)
                             ))))

(defun lsp-bridge-linenote-delete ()
  (interactive)
  (lsp-bridge-call-async "linenote_delete" (my-project-root) (file-local-name (buffer-file-name))))

(defcustom lsp-bridge-linenote-update-hook '()
  "The hook for diagnostics updates."
  :type 'hook
  :group 'lsp-bridge)

(defun lsp-bridge-linenote--render (filepath filehost diagnostics diagnostic-count)
  (lsp-bridge--with-file-buffer filepath filehost
                                (setq-local lsp-bridge-linenote-count diagnostic-count)

                                (setq-local lsp-bridge-linenote-records diagnostics)

                                (run-hooks 'lsp-bridge-linenote-update-hook)
                                )
  )

(defun flymake-linenote (report-fn &rest _args)
  "A flymake backend for `lsp-bridge-diagnostic'.
Add this to `flymake-diagnostic-functions' to enable it.
Calls REPORT-FN directly."
  (cl-loop for diag in (seq-remove
                        (lambda (x)
                          (member (plist-get x :severity)
                                  lsp-bridge-diagnostic-hide-severities))
                        lsp-bridge-linenote-records)
           collect
           (flymake-make-diagnostic
            (current-buffer)
            (acm-backend-lsp-position-to-point
             (plist-get (plist-get diag :range) :start))
            (acm-backend-lsp-position-to-point
             (plist-get (plist-get diag :range) :end))
            (cl-case (plist-get diag :severity)
              (1 :error)
              (2 :warning)
              ((3 4) :note))
            (concat (when-let ((code (plist-get diag :code)))
                      (concat "[" (or (and (stringp code) code)
                                      (prin1-to-string code))
                              "] "))
                    (plist-get diag :message)
                    (when-let ((server (plist-get diag :server-name)))
                      (concat " (" (propertize server 'face
                                               'flymake-bridge-server)
                              ")"))))
           into diags
           finally (funcall report-fn diags)))

;; (lsp-bridge-linenote-close-file-hook)
(provide 'lsp-bridge-linenote)
;;; lsp-bridge-linenote.el ends here