# Helm Dash

## What's it

This package uses dash docsets inside emacs to browse documentation. It's inspired by the great app [Dash](http://www.kapeli.com/dash).

![](https://raw.github.com/areina/helm-dash/master/misc/helm-dash.gif)

## What's not

If you're looking for dash.el, the list library, please go to [dash.el](http://www.github.com/magnars/dash.el)



## Installation

- Put sqlite.el in your load path (in this same repo).
- Install helm.
- Install helm-dash.

## Usage

`m-x helm-dash RET` will run helm with your active docsets loaded. Typing substrings of what you search will find-as-you-type. 

- The search starts from 3 chars.
- Install new docsets with m-x helm-dash-install-docset
- After installing a new docset, add the name of the docset to `helm-dash-active-docsets'
