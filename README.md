# Helm Dash

## What's it

This package uses dash docsets inside emacs to browse documentation. It's inspired by the great app [Dash](http://www.kapeli.com/dash).

![](https://raw.github.com/areina/helm-dash/master/misc/helm-dash.gif)

## What's not

If you're looking for dash.el, the list library, please go to [dash.el](http://www.github.com/magnars/dash.el)


## Requirements

- [helm](https://github.com/emacs-helm/helm)
- [sqlite](https://github.com/cnngimenez/sqlite.el)

## Installation

It's available on [MELPA](http://melpa.milkbox.net).

`m-x package-install helm-dash RET`

## Usage

`m-x helm-dash RET` will run helm with your active docsets loaded. Typing substrings of what you search will find-as-you-type.

- The search starts from 3 chars.
- Install new docsets with m-x helm-dash-install-docset
- After installing a new docset, add the name of the docset to `helm-dash-active-docsets'

## Sets of Docsets

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

Dash accepts a few different docset types while helm-dash only understands the [simplest ones](http://kapeli.com/docsets#dashDocset).

## Authors

- Toni Reina <areina0@gmail.com>
- Raimon Grau <raimonster@gmail.com>
