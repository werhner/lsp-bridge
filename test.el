(defun citre-readtags--get-cmd
    (tagsfile &optional name match case-fold filter sorter action)
  "Get lines in TAGSFILE using readtags.
See `citre-readtags-get-tags' to know about NAME, MATCH, CASE-FOLD,
FILTER, and SORTER.  ACTION can be nil, to get regular tags, or
any valid actions in readtags, e.g., \"-D\", to get pseudo tags."
  (let* ((match (or match 'exact))
         (extras (concat
                  "-Ene"
                  (pcase match
                    ('exact "")
                    ('prefix "p")
                    (_ (error "Unexpected value of MATCH")))
                  (if case-fold "i" "")))
         (tagsfile (substring-no-properties tagsfile))
         (name (when name (substring-no-properties name)))
         (filter (citre-readtags--strip-text-property-in-list filter))
         (sorter (citre-readtags--strip-text-property-in-list sorter))
         inhibit-message
         cmd)
    ;; Program name
    (push (or citre-readtags-program "readtags") cmd)
    ;; Read from this tags file
    (push "-t" cmd)
    (push (file-local-name tagsfile) cmd)
    ;; Filter expression
    (when filter (push "-Q" cmd) (push (format "%S" filter) cmd))
    (when sorter (push "-S" cmd) (push (format "%S" sorter) cmd))
    ;; Extra arguments
    (push extras cmd)
    ;; Action
    (if action (push action cmd)
      (if (or (null name) (string-empty-p name))
          (push "-l" cmd)
        (push "-" cmd)
        (push name cmd)))
    (nreverse cmd)))

(let ((cmd nil)
      (parse-option nil))
  (letf! ((defun citre-readtags--get-lines (tagsfile &optional name match case-fold filter sorter action)
            (progn
              (setq cmd (citre-readtags--get-cmd tagsfile name match case-fold filter sorter action))
              '(t)))
          (defun citre-readtags--parse-line (line &optional tagsfile-info require optional exclude require-ext optional-ext ext-dep parse-all-fields)
            (setq parse-option `(,tagsfile-info ,require ,optional ,exclude ,require-ext ,optional-ext ,ext-dep ,parse-all-fields))))
    (citre-tags-get-completions "DEFUN")
    (message "%S" (string-join cmd " "))
    (message "%S" parse-option)
    nil))

(setq test-tags (citre-tags-get-completions "stretch" "/Users/bytedance/code/emacs/tags"))
(message "%S" (mapcar #'citre-capf--make-candidate test-tags))


