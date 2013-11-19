# helm-gtags.el

## Introduction
`helm-gtags.el` is GNU GLOBAL helm interface.

**helm-gtags.el** is not compatible **anything-gtags.el**.
But `helm-gtags.el` is designed for faster search than `anything-gtags.el`.

`anything-gtags.el` is slow in large source tree such as Linux kernel,
FreeBSD, Android etc. Because `anything-gtags.el` creates candidates
by processing output of `gtags.el`. `helm-gtags.el` creates candidates
by itself, so `helm-gtags.el` is faster than `anything-gtags.el`.


## Screenshot

![helm-gtags](image/helm-gtags.png)


## Requirements
* Emacs 23 or higher.
* helm 1.0 or higher
* GNU Global 5.7.1 or higher

`helm-gtags.el` does not require `gtags.el`.


## Basic Usage

#### `helm-gtags-mode`

Enable `helm-gtags-mode`.

#### `helm-gtags-find-tag`

Input tag name and move to the definition.

#### `helm-gtags-find-tag-from-here`

Find tag from here and move to its definition (gnu global --from-here).

#### `helm-gtags-find-rtag`

Input tag name and move to the referenced point.

#### `helm-gtags-find-symbol`

Input symbol and move to the locations.

#### `helm-gtags-find-tag-and-symbol`

 `helm-gtags-find-tag` and  `helm-gtags-find-symbol`
 
#### `helm-gtags-find-files`

Input file name and open it.

#### `helm-gtags-select`

Tag jump using gtags and helm

First list all symbol to select, after you select the symbol
use it as keyword calling one of
  `helm-gtags-find-symbol`
  `helm-gtags-find-rtag`
  `helm-gtags-find-tag`

### `helm-gtags-complete`

Complete symbol at point(help writing code).

#### `helm-gtags-update-tags`

Update TAG file. Default is update only current file.
You can update all files with `C-u` prefix.
You can generate new tags with `C-uC-u` prefix.

#### `helm-gtags-parse-file`

Show symbols in current file like `gtags-parse-file`. You can choose
any files with `C-u` prefix.

## Customize Variables

### `helm-gtags-tag-location-alist`
You could add your lib directory here ,like
```
(setq helm-gtags-tag-location-alist
      '((c-mode  "/usr/include/" "/usr/kernel/")
        (c++-mode  "/path/of/tag/2/" "/path/of/tag/3/")))
you neednot set this if you just use one tag
        
```
of cource  you should run :
```
gtags
```
in these directories.
then 
  `helm-gtags-find-symbol`
  `helm-gtags-find-rtag`
  `helm-gtags-find-tag`
  `helm-gtags-find-tag-from-here`
  `helm-gtags-find-files`
  `helm-gtags-select`
could find tag rtags symbol files in several directory with one command
#### `helm-gtags-path-style`

File path style, `'root` or `'relative` or `'absolute`(Default is `'root`)

#### `helm-gtags-ignore-case`

Ignore case for searching flag (Default is `nil`)

#### `helm-gtags-read-only`

Open file as readonly, if this value is `non-nil`(Default is `nil`).


#### `helm-gtags-auto-update`

If this variable is non-nil, TAG file is updated after saving buffer.

### History Navigate Feature 
do not support history navigate feature directly.
if you want this feature, you just need use
helm-gtags-select-before-hook and helm-gtags-goto-line-before-hook hooks,

#### for eaxmple using bm.el helm-bm.el

https://github.com/joodland/bm
https://github.com/jixiuf/helm-bm
you just need this lines:

```elisp
   (autoload 'bm-toggle   "bm" "Toggle bookmark in current buffer." t)
   (autoload 'bm-next     "bm" "Goto bookmark."                     t)
   (autoload 'bm-previous "bm" "Goto previous bookmark."            t)
   (global-set-key (kbd "<C-f2>")   'bm-toggle)
   (global-set-key (kbd "M-,")      'bm-next)
   (global-set-key (kbd "M-/")      'bm-previous)
   (setq bm-highlight-style 'bm-highlight-only-fringe)
   (setq bm-cycle-all-buffers t)
   
   (autoload 'helm-bm "helm-bm" "List All Bookmarks with helm.el."            t)
   (global-set-key (kbd "M-*")   'helm-bm)
   
   
   (require 'helm-gtags)
   (autoload 'bm-bookmark-add "bm" "Add bookmark at current line.")
   (add-hook 'helm-gtags-goto-line-before-hook 'bm-bookmark-add)
   ;;; and other helm-gtags.el configuration
   
   
   ;;;  but bm-next bm-previous is not follow LIFO order(that is, most
   recently set ones come first, oldest ones come last) 
```
#### Another example use emacs default bookmark system
 emacs default bookmark doesnot provide bookmark-next or bookmark-previous
 so I write https://github.com/jixiuf/bookmark-cycle
 and with the helm-bookmark.el in https://github.com/emacs-helm/helm

```elisp
;; first: you should
      (require 'bookmark)
      (setq bookmark-sort-flag nil)           ;
      (setq bookmark-bmenu-file-column 120)   ;
      
;; then:
      (global-set-key (kbd "<C-f2>") 'bookmark-cycle-push)
      (global-set-key (kbd "M-,") 'bookmark-cycle-next)
      (global-set-key (kbd "M-/") 'bookmark-cycle-previous)
      
      (require 'helm-bookmark)
      (setq helm-bookmark-show-location t)  ;
      (define-key global-map (kbd "M-*") 'helm-pp-bookmarks);; in helm-bookmark.el

     (require 'helm-gtags)
      (add-hook 'helm-gtags-goto-line-before-hook 'bookmark-cycle-push)
      
      

```
     these package lets you cycle among bookmarks if you donot want use bookmark-cycle.el.
     http://www.emacswiki.org/emacs/BookmarkPlus
     http://www.emacswiki.org/emacs/bookmark-add.el
     http://www.emacswiki.org/emacs/BookMarks
     
## Sample Configuration

```elisp
(require 'helm-config)
(require 'helm-gtags)

;;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; customize
(setq helm-gtags-path-style 'relative)
(setq helm-gtags-ignore-case t)
(setq helm-gtags-read-only t)
(setq helm-gtags-auto-update t)

;;you neednot set this if you just use one tag
(setq helm-gtags-tag-location-alist
      '((c-mode  "/usr/include/" "/usr/kernel/")
        (c++-mode  "/path/of/tag/2/" "/path/of/tag/3/")))

(eval-after-load "helm-gtags" '(add-to-list 'helm-for-files-preferred-list helm-source-gtags-files t))

;; key bindings
    (add-hook 'helm-gtags-mode-hook
              '(lambda ()
                 (local-set-key [(meta return)] 'helm-gtags-complete)
                 (local-set-key (kbd "M-.") 'helm-gtags-find-tag-and-symbol)
                 (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
                 (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
                 (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
                 (local-set-key (kbd "M-g M-p") 'helm-gtags-parse-file)
                 (local-set-key (kbd "C-c C-f") 'helm-gtags-find-files)))
```
