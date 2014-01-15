# Helm Dash

## What's it

This package uses [Dash](http://www.kapeli.com/dash) docsets inside emacs to browse
documentation.

![](https://raw.github.com/areina/helm-dash/master/misc/helm-dash.gif)

## What's not

If you're looking for dash.el, the list library, please go to
[dash.el](http://www.github.com/magnars/dash.el)


## Requirements

- [helm](https://github.com/emacs-helm/helm)
- [esqlite](https://github.com/mhayashi1120/Emacs-esqlite)

Previously, we were using [sqlite](https://github.com/cnngimenez/sqlite.el)
but we had some problems with it because it was returning different
results for the same sql query.

## Installation

It's available on [MELPA](http://melpa.milkbox.net).

`m-x package-install helm-dash RET`

## Usage

`m-x helm-dash RET` will run helm with your active docsets
loaded. Typing substrings of what you search will find-as-you-type.

- The search starts from 3 chars.
- Install new docsets with m-x helm-dash-install-docset
- After installing a new docset, add the name of the docset to
  `helm-dash-common-docsets' or in 'helm-dash-docsets' (which is ment
  to be buffer local)


The command `helm-dash-reset-connections` will clear the connections
to all sqlite db's. Use it in case of errors when adding new docsets.
The next call to `helm-dash` will recreate them.

## Sets of Docsets

### Common docsets

`helm-dash-common-docsets' is a list that should contain the docsets
to be active always. In all buffers.

### Buffer local docsets

Different subsets of docsets can be activated depending on the
buffer. For the moment (it may change in the future) we decided it's a
plain local variable you should setup for every different
filetype. This way you can also do fancier things like project-wise
docsets sets.

``` elisp
(defun go-doc ()
  (interactive)
  (setq-local helm-dash-docsets '("Go")))

(add-hook 'go-mode-hook 'go-doc)
```

## Caveats

helm-dash has been tested only in linux.

## Authors

- Toni Reina <areina0@gmail.com>
- Raimon Grau <raimonster@gmail.com>
