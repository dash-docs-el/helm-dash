;;; helm-dash.el --- Offline documentation browser for +150 APIs using Dash docsets.  -*- lexical-binding: t; -*-
;; Copyright (C) 2013-2014  Raimon Grau
;; Copyright (C) 2013-2014  Toni Reina

;; Author: Raimon Grau <raimonster@gmail.com>
;;         Toni Reina  <areina0@gmail.com>
;;         Bryan Gilbert <bryan@bryan.sh>
;;
;; URL: https://github.com/dash-docs-el/helm-dash
;; Version: 1.3.0
;; Package-Requires: ((emacs "24.4") (dash-docs "1.4.0") (helm "1.9.2") (cl-lib "0.5"))
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
;;
;;; Commentary:
;;
;; Clone the functionality of dash using helm foundation.  Browse
;; documentation via dash docsets.
;;
;; M-x helm-dash
;; M-x helm-dash-at-point
;;
;; More info in the project site https://github.com/areina/helm-dash
;;
;;; Code:

(eval-when-compile (require 'cl))
(require 'dash-docs)
(require 'helm)
(require 'helm-multi-match)

;;; Customize

(defgroup helm-dash nil
  "Search Dash docsets using helm."
  :prefix "helm-dash-"
  :group 'applications)


(defvaralias 'helm-dash-docsets-path 'dash-docs-docsets-path)
(defvaralias 'helm-dash-docsets-url 'dash-docs-docsets-url)
(defvaralias 'helm-dash-min-length 'dash-docs-min-length)
(defvaralias 'helm-dash-candidate-format 'dash-docs-candidate-format)
(defvaralias 'helm-dash-enable-debugging 'dash-docs-enable-debugging)
(defvaralias 'helm-dash-browser-func 'dash-docs-browser-func)
(defvaralias 'helm-dash-common-docsets 'dash-docs-common-docsets)
(defvaralias 'helm-dash-ignored-docsets 'dash-docs-ignored-docsets)

(defalias 'helm-dash--candidate 'dash-docs--candidate)
(defalias 'helm-dash--run-query 'dash-docs--run-query)
(defalias 'helm-dash-actions 'dash-docs-actions)
(defalias 'helm-dash-activate-docset 'dash-docs-activate-docset)
(defalias 'helm-dash-create-buffer-connections 'dash-docs-create-buffer-connections)
(defalias 'helm-dash-create-common-connections 'dash-docs-create-common-connections)
(defalias 'helm-dash-deactivate-docset 'dash-docs-deactivate-docset)
(defalias 'helm-dash-initialize-debugging-buffer 'dash-docs-initialize-debugging-buffer)
(defalias 'helm-dash-install-docset 'dash-docs-install-docset)
(defalias 'helm-dash-install-docset-from-file 'dash-docs-install-docset-from-file)
(defalias 'helm-dash-installed-docsets 'dash-docs-installed-docsets)
(defalias 'helm-dash-install-user-docset 'dash-docs-install-user-docset)
(defalias 'helm-dash-maybe-narrow-docsets 'dash-docs-maybe-narrow-docsets)
(defalias 'helm-dash-reset-connections 'dash-docs-reset-connections)

(defvar helm-dash-history-input nil)

(defun helm-dash-search ()
  "Iterates every `helm-dash-connections' looking for the `helm-pattern'."
  (let ((connections (helm-dash-maybe-narrow-docsets helm-pattern)))
    (cl-loop for docset in connections
             append (cl-loop for row in (helm-dash--run-query docset helm-pattern)
                             collect (helm-dash--candidate docset row)))))

(make-obsolete #'helm-dash-search nil "1.3.0")

(defun helm-dash--build-source (docset)
  "Build a Helm source for DOCSET."
  (lexical-let ((docset docset))
    (helm-build-sync-source (car docset)
      :action-transformer #'helm-dash-actions
      :candidates (lambda ()
                    (cl-loop for row in (helm-dash--run-query docset helm-pattern)
                             collect (helm-dash--candidate docset row)))
      :volatile t
      :persistent-help "View doc"
      :requires-pattern helm-dash-min-length)))

(defun helm-dash--sources-narrowed-docsets ()
  "Return a list of Helm sources for narrowed docsets.

Narrowed docsets are those returned by
`helm-dash-maybe-narrow-docsets'."
  (let ((connections (helm-dash-maybe-narrow-docsets helm-pattern)))
    (cl-loop for docset in connections
             append (list (helm-dash--build-source docset)))))

;;; Autoloads

;;;###autoload
(defun helm-dash (&optional input-pattern)
  "Bring up a `helm-dash' search interface.
If INPUT-PATTERN is non-nil, use it as an initial input in helm search."
  (interactive)
  (helm-dash-initialize-debugging-buffer)
  (helm-dash-create-common-connections)
  (helm-dash-create-buffer-connections)
  (helm :sources (helm-dash--sources-narrowed-docsets)
        :buffer "*helm-dash*"
        :prompt "Doc for: "
        :history 'helm-dash-history-input
        :input input-pattern
        :helm-candidate-number-limit 1000))

;;;###autoload
(defun helm-dash-at-point ()
  "Bring up a `helm-dash' search interface with symbol at point."
  (interactive)
  (helm-dash
   (substring-no-properties (or (thing-at-point 'symbol) ""))))

(provide 'helm-dash)

;;; helm-dash.el ends here
