# Brf-Mode

Brf-Mode adds functionality from the old DOS editor
[Brief](https://en.wikipedia.org/wiki/Brief_(text_editor)) to Emacs.

## Summary

This package is not an emulation of Brief (there are plenty of those
already), but rather provides an accurate implementation in Emacs of
specific features that I miss from Brief.

The emphasis is on accurately implementing these specific features in
Emacs rather than doing what Brief emulations tend to do, which
is mapping the Brief key-sequences to similar functions in
Emacs.

Principally these features are:

* [Line-mode cut and paste](#line--column-mode-cut-and-paste)
* [Column-mode cut and paste](#line--column-mode-cut-and-paste)
* [Fully reversible paging and scrolling](#reversible-paging-and-scrolling)
* [Temporary bookmarks](#temporary-bookmarks)
* [Cursor motion undo](#cursor-motion-undo)
* [Easy window management](#easy-window-management)

However they have been implemented in an Emacs-style. This means the
functions respond to prefix args and where they override Emacs
functions, they live on the Emacs key bindings as well as the original
Brief keys.

Moreover, functionality has been extended to those parts of Emacs
that were never part of Brief.  For example, text cut/copied in
line or column-mode can be saved/recalled in registers.

Also some functionality was never part of Brief nor Emacs (for example
[`List Bookmarks`](#list-bookmarks) and the mode uses the prefix `C-c
C-b` for such commands.

## Setup

### Installation

* Manual

```emacs-lisp
   (require 'brf)
   (brf-mode) 
   ;; or set via customize
```

* Package 

Coming soon...

### Customisation
* Options

```emacs-lisp
   (customize-group 'brf)
```

Customisable options are:

1. Bookmark Face
2. Bookmark Number face (when shown in Fringe)
3. Enable Brf-Mode
4. Mode-line string (including hiding)
5. Enable [Cursor Motion Undo](#cursor-motion-undo)

* Key mapping
 
 Default key mappings can be changed by modifying `brf-mode-map` in the mode hook:

```emacs-lisp
   (add-hook 'brf-mode-hook
     (lambda ()
       (define-key brf-mode-map ...)))
```

## Features

### <a name="line--column-mode-cut-and-paste"></a>Line & Column Mode Cut and Paste

* Mark regions by whole line or column.
* If no region is marked, the copy and kill commands operate on the
current line.
* Yanked text is inserted in line or column mode, if that's how it was
marked.
* Text in Line or Column mode can be stored and recalled from registers, 
as well as the kill-ring.

![line-mode](https://bitbucket.org/MikeWoolley/brf-mode/raw/master/images/line-mode.png)

![column-mode](https://bitbucket.org/MikeWoolley/brf-mode/raw/master/images/column-mode.png)

| Key         | Action                        |
|-------------|-------------------------------|
| M-l         | Start line marking            |
| M-c         | Start column marking          |
| M-m         | Start normal marking          |
| kp-add      | Copy Line or Region           |
| M-w         | Copy Line or Region           |
| kp-subtract | Kill Line or Region           |
| C-w         | Kill Line or Region           |
| kp-ins      | Yank                          |
| C-y         | Yank                          |
| M-y         | Yank Pop                      |
|             |                               |
| C-c C-b C-w | Copy to Register              |
| C-c C-b C-y | Insert Register               |
|             |                               |
| M-d         | Delete Line                   |
| delete      | Delete Region or Char         |
|             |                               |
| RET         | Newline and Indent            |
| C-j         | Newline                       |
| C-RET       | Open New Line                 |
| Tab         | Indent                        |

### <a name="reversible-paging-and-scrolling">Reversible Paging and Scrolling

* Paging and scrolling respect relative screen row and absolute column.
* Paging up and then down again returns point to the same original position.

| Key    | Action                        |
|--------|-------------------------------|
| next   | page-down                     |
| C-v    | page-down                     |
| prior  | page-up                       |
| M-v    | page-up                       |
| M-down | scroll-down                   |
| M-up   | scroll-up                     |
| home   | Beginning of Line/Page/Buffer |
| end    | End of Line/Page/Buffer       |

### <a name="temporary-bookmarks">Temporary Bookmarks

* 10 bookmarks can be set and navigated between. 
* They can also be moved and deleted.
* They are temporary in the sense they don't persist between
invocations of Emacs.
* As an extension to Brief, bookmark lines are highlighted in colour
 (customisable).
* If the package `fringe-helper` is installed, the bookmark number is
 put in the fringe (which otherwise shows as a tooltip).
* If the package `generic-menu` is installed, bookmarks can be listed
 & chosen from a menu, [see below](#list-bookmarks). 
 This is also an extension to Brief.
* Other extensions are a command to allocate the next free bookmark
  and one to delete all bookmarks.

![bookmarks](https://bitbucket.org/MikeWoolley/brf-mode/raw/master/images/bookmarks.png)

| Key                 | Action                               |
|---------------------|--------------------------------------|
| M-0 to M-9          | Drop bookmark 0-9 at point           |
| With prefix arg C-u | Removes bookmark.                    |
| M-=                 | Goto Next Bookmark                   |
| M-kp-add            | Goto Next Bookmark                   |
| M-kp-subtract       | Goto Previous Bookmark               |
| M--                 | Goto Previous Bookmark               |
| M-j                 | Jump-to-Bookmark                     |
|                     |                                      |
| C-c C-b C-k         | Delete All Bookmarks                 |
| C-c C-b C-l         | List Bookmarks                       |
| C-c C-b C-n         | Goto Next Bookmark                   |
| C-c C-b C-p         | Goto Previous Bookmark               |
| C-c C-b =           | Allocate Next Free Bookmark at Point |

### <a name="list-bookmarks">List Bookmarks

![list-bookmarks](https://bitbucket.org/MikeWoolley/brf-mode/raw/master/images/list-bookmarks.png)

| key  | Action                    |
|------|---------------------------|
| RET  | Jump to bookmark at point |
| d    | Delete bookmark at point  |
| k    | Delete All bookmarks      |
| q    | Quit                      |

### <a name="cursor-motion-undo">Cursor Motion Undo

* Cursor motion, without any buffer changes, is recorded as an
undo-able (& redo-able) action.
* This works with both built-in Emacs Undo and also with the `Redo.el`
& `Redo+.el` packages. I haven't tested it with the plethora of other Undo
packages - it should work, but you never know!
* It is turned off by default (unlike in Brief), but can enabled by Customise
option `b-undo-enable`.

| Key         | Action                                 |
|-------------|----------------------------------------|
| kp-multiply | Undo                                   |
| M-u         | Undo                                   |
| M-r         | Redo (if `redo` or `redo+` installed). |

### <a name="easy-window-management">Easy Window Management

* Create, Switch, Resize and Delete arbitrary windows with simple
  keystrokes.

| Key                        | Action                        |
|----------------------------|-------------------------------|
| S- [up, down, right, left] | Switch to Window in Direction |
| f1 [up, down, right, left] | Switch to window in Direction |
| f2 [up, down, right, left] | Resize Window in Direction    |
| f3 [up, down, right, left] | Create Window in Direction    |
| f4 [up, down, right, left] | Delete Window in Direction    |
| C-f4                       | Delete Current Window         |
| S-f4                       | Delete Other Windows          |
