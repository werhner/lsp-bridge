;;; acm-backend-gtags.el --- acm gtags support -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(defcustom acm-backend-gtags-candidate-min-length 3
  "Minimal length of candidate."
  :type 'integer
  :group 'acm-backend-gtags)

(defcustom acm-enable-gtags t
  "Popup gtags completions when this option is turn on."
  :type 'boolean
  :group 'acm-backend-gtags)

(defvar-local acm-backend-gtags-items nil)

(defun acm-backend-gtags-candidates (keyword)
  (acm-with-cache-candidates
   acm-backend-gtags-cache-candiates
   (when (and acm-enable-gtags
              (>= (length keyword) acm-backend-gtags-candidate-min-length))
     acm-backend-gtags-items)))

(defun acm-backend-gtags-clean ()
  (setq-local acm-backend-gtags-items nil)
  (setq-local acm-backend-gtags-cache-candiates nil))

(defun acm-backend-gtags-candidate-expand (candidate-info bound-start &optional preview)
  (if preview
      (acm-preview-create-overlay bound-start (point) (plist-get candidate-info :label))
    (delete-region bound-start (point))
    (insert (plist-get candidate-info :label))))

(provide 'acm-backend-gtags)

;;; acm-backend-gtags.el ends here
