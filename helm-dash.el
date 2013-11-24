(load-file "sqlite.el")
(require 'sqlite)

(defun helm-dash-search ()
 (let ((conn (sqlite-init "Vim.docset/Contents/Resources/docSet.dsidx"))
       (db "searchIndex")
       (res nil))
   (setq res (sqlite-query conn (format "SELECT * FROM %s WHERE \"name\" like \"%%s%\" where " db helm-pattern)))
   (sqlite-bye conn)

   (message res)
   ))

(defvar helm-source-dash-search
  '((name . "Dash")
    (volatile)
    (delayed)
    (multiline)
    (requires-pattern . 2)
    (candidates-process . helm-dash-search)
    (action-transformer . helm-dash-actions)))

;;;###autoload
(defun helm-dash ()
  "Bring up a Spotify search interface in helm."
  (interactive)
  (helm :sources '(helm-source-dash-search)
	:buffer "*helm-dash*"))

(provide 'helm-dash)
