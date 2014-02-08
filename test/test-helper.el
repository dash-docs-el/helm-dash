(require 'f)

(defvar helm-dash-test-path
  (f-dirname (f-this-file)))

(defvar helm-dash-code-path
  (f-parent helm-dash-test-path))

(require 'helm-dash (f-expand "helm-dash.el" helm-dash-code-path))

