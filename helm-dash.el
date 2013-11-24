(load-file "sqlite.el")

(require 'helm)
(require 'sqlite)

(defun helm-dash-search ()
 (let ((conn (sqlite-init "Go.docset/Contents/Resources/docSet.dsidx"))
       (db "searchIndex")
       (res nil))
   (setq res (sqlite-query conn (format "SELECT type, name, path FROM %s WHERE \"name\" like \"%%%s%%\"" db helm-pattern)))
   (sqlite-bye conn)
   (mapcar (lambda (x)
                   (cons (cadr x) (format "%s%s%s"
                                          default-directory
                                          "Go.docset/Contents/Resources/Documents/"
                                          (caddr x))))
           res)))

;; (defun helm-dash-browse-webdoc (webdoc)
;;   (browse-url-chromium  (format "%s/Go.docset/Contents/Resources/Documents/godoc.org/code.google.com/p/go.crypto/ssh.html#VDSUSP" default-directory)))

(defun helm-dash-actions (actions doc-item) `(("Go to doc" . browse-url))
  )

(defvar helm-source-dash-search
  '((name . "Dash")
    (volatile)
    (delayed)
    (multiline)
    (requires-pattern . 3)
    (candidates-process . helm-dash-search)
    (action-transformer . helm-dash-actions)))

;;;###autoload
(defun helm-dash ()
  "Bring up a Spotify search interface in helm."
  (interactive)
  (helm :sources '(helm-source-dash-search)
	:buffer "*helm-dash*"))

(provide 'helm-dash)
