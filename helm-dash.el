;;; helm-dash.el --- Helm extension to search dash docsets

;; Copyright (C) 2013  Raimon Grau
;; Copyright (C) 2013  Toni Reina

;; Author: Raimon Grau <raimonster@gmail.com>
;;         Toni Reina  <areina0@gmail.com>
;; Keywords: docs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; When We have many active-docsets and the result is only from one
;; of those, the result is super slow.  have to investigate


;;; Code:



(provide 'helm-dash)
;;; helm-dash.el ends here


(load-file "sqlite.el")

(require 'helm)
(require 'sqlite)

(defcustom helm-dash-docsets-path "/home/kidd/programmingStuff/d/" "Default path for docsets.")
(defcustom helm-dash-active-docsets '( "Go" "HttpLuaModule") ".")

(defun connect-to-docset (docset)
      (sqlite-init (format
                    "%s.docset/Contents/Resources/docSet.dsidx"
                    docset)))

;;; create conses like ("Go" . connection)
(setq helm-dash-connections
      (mapcar (lambda (x)
                (cons x (connect-to-docset x)))
              helm-dash-active-docsets))

(defun helm-dash-search ()
  "Iterates every connection looking for the `helm-pattern'."
  (let ((db "searchIndex")
        (full-res (list)))
    (dolist (docset helm-dash-connections)
      (let ((res
             (sqlite-query (cdr docset)
                           (format
                            "SELECT t.type, t.name, t.path FROM %s t WHERE \"name\" like \"%%%s%%\" order by lower(t.name)"
                            db helm-pattern))))
        ;; how to do the appending properly?
        (setq full-res
              (append full-res
                      (mapcar (lambda (x)
                                (cons (cadr x) (format "%s%s%s%s"
                                                       "file://"
                                                       helm-dash-docsets-path
                                                       (format "%s.docset/Contents/Resources/Documents/" (car docset))
                                                       (caddr x))))
                              res)))))
    full-res))

;; (defun helm-dash-browse-webdoc (webdoc)
;;   (browse-url-chromium  (format "%s/Go.docset/Contents/Resources/Documents/godoc.org/code.google.com/p/go.crypto/ssh.html#VDSUSP" default-directory)))

(defun helm-dash-actions (actions doc-item) `(("Go to doc" . browse-url)))

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

;;; helm-dash.el ends here
