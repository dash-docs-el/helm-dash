(load-file "sqlite.el")
(require 'sqlite)

(let ((conn (sqlite-init "Vim.docset/Contents/Resources/docSet.dsidx")))
        (setq res (sqlite-query conn "SELECT * FROM searchIndex limit 4"))
        (sqlite-bye conn))
res
