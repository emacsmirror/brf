# Brf-mode

Brf-mode adds functionality from the legendary programmer's editor [Brief](https://en.wikipedia.org/wiki/Brief_%28text_editor%29) to Emacs.

This package is not an emulation of Brief (there are plenty of those already), but rather provides an accurate implementation in Emacs of specific features that I miss from Brief.

The emphasis is on accurately implementing these specific features in Emacs rather than doing what Brief emulations tend to do, which is mapping the Brief key-sequences to somewhat similar functions in Emacs.

The features are:

-   [Line-mode cut and paste](#line-and-column-mode-cut-and-paste)
-   [Column-mode cut and paste](#line-and-column-mode-cut-and-paste)
-   [Fully reversible paging and scrolling](#reversible-paging-and-scrolling)
-   [Temporary bookmarks](#temporary-bookmarks)
-   [Cursor motion undo](#cursor-motion-undo)
-   [Easy window management](#easy-window-management)

They have been implemented in an Emacs-style. This means the functions respond to prefix args and where they override Emacs functions, they live on the Emacs key bindings as well as the original Brief keys.

Moreover, functionality has been extended to those parts of Emacs that were never part of Brief. For example, text cut/copied in line or column-mode can be saved/recalled in registers.

Some functionality was neither part of Brief nor Emacs (for example [List Bookmarks](#list-bookmarks)) and the mode generally uses the prefix `C-c C-b` for such commands.

Brf-mode puts a `Brf` sub-menu under `Edit`. In keeping with Brief's minimalist ethos, the menu only houses these more difficult to access commands and also has links to preferences, help, the manual and project website.


# Setup


## Installation

[![img](https://melpa.org/packages/brf-badge.svg)](https://melpa.org/#/brf) [![img](https://stable.melpa.org/packages/brf-badge.svg)](https://stable.melpa.org/#/brf)

The easiest way is to install the `brf` package from MELPA, however Brf-mode can also be installed manually if desired:

-   Installation from MELPA
    1.  Make sure [melpa is in your package archives list](https://melpa.org/#/getting-started).
    2.  `M-x package-install brf`

-   Manual Installation
    1.  Download the package to a directory and add it to your `load-path`:
        
        ```emacs-lisp
        (add-to-list 'load-path <install directory>)
        ```
    
    2.  Install the Info manual [optional].
        
        ```shell
        $ cd <install directory>
        $ install-info brf-mode.info
        ```
        
        ```emacs-lisp
        (add-to-list 'Info-directory-list <install directory>)
        ```

-   Enable Brf-mode
    
    This can be done via [Customize](#customisation) or adding code to your startup file:
    
    ```emacs-lisp
    (brf-mode)
    ```
    
    Choosing "Enable Brf-mode" from the "Brf" menu, toggles the mode on or off for the current session.


<a id="customisation"></a>

## Customisation

`M-x customize-group brf`

-   Options
    
    Customisable options are:
    
    1.  Bookmark Face
    2.  Bookmark Number face (when shown in Fringe)
    3.  Enable Brf-mode
    4.  Mode-line string (including hiding)
    5.  Enable [Cursor Motion Undo](#cursor-motion-undo)

-   Key mapping
    
    Default key mappings can be changed by modifying `brf-mode-map` in the mode hook.
    
    As an example, here's what I'm using myself:
    
    ```emacs-lisp
    (add-hook 'brf-mode-hook
    	  (lambda ()
    	    (define-key brf-mode-map "\M-r" 'redo)                ; Redo from redo+.el
    	    (define-key brf-mode-map "\M-a" nil)                  ; Don't use Brief Alt-a for marking
    	    (define-key brf-mode-map "\M-m" nil)                  ; Don't use Brief Alt-m for marking
    	    (define-key brf-mode-map "\C-xl" 'downcase-word)      ; Shadowed by Alt-l
    	    (define-key brf-mode-map "\C-xu" 'upcase-word)        ; Shadowed by Alt-u
    	    (define-key brf-mode-map "\C-xw" 'capitalize-word)))  ; Shadowed by Alt-c
    ```


## Dependencies

Brf-mode doesn't have any hard dependencies, but installing the following optional packages enables some extra capabilities:

-   [fringe-helper](https://melpa.org/#/fringe-helper): When installed, bookmark numbers show in the fringe. Installing Brf-mode via `package-install` automatically installs `fringe-helper`.

-   [pkg-info](https://melpa.org/#/pkg-info): When installed, `(brf-version)` shows the package version as well as the Brf-mode version.


<a id="compatibility"></a>

## Compatibility

The MELPA package (Brf-mode version "v1.16-MELPA" onward) requires Gnu Emacs 24.3, due to the requirements of being a package.

Earlier versions of Brf-mode will however work on older versions of Gnu Emacs and also on XEmacs:

-   Use "v1.16" for Gnu Emacs versions 21 -> 24.2.
-   Use "v1.08" for XEmacs and Gnu Emacs 20 & earlier.

These and any other version can be download from the [Brf-mode website](https://bitbucket.org/MikeWoolley/brf-mode/downloads/?tab=tags) and installed manually.


# Features


<a id="line-and-column-mode-cut-and-paste"></a>

## Line and Column Mode Cut and Paste

-   Mark regions by whole line or column.
-   If no region is marked, the copy and kill commands operate on the current line.
-   Yanked text is inserted in line or column mode, if that's how it was marked.
-   Text in Line or Column mode can be stored and recalled from registers, as well as the kill-ring.

![img](https://bitbucket.org/MikeWoolley/brf-mode/raw/master/images/line-mode.png "Line Mode")

![img](https://bitbucket.org/MikeWoolley/brf-mode/raw/master/images/column-mode.png "Column Mode")

| Key         | Action                  |
|----------- |----------------------- |
| M-l         | Start line marking      |
| M-c         | Start column marking    |
| M-m         | Start character marking |
| M-a         | Start character marking |
| kp-add      | Copy Line or Region     |
| M-w         | Copy Line or Region     |
| kp-subtract | Kill Line or Region     |
| C-w         | Kill Line or Region     |
| kp-ins      | Yank                    |
| C-y         | Yank                    |
| M-y         | Yank Pop                |
|             |                         |
| C-c C-b C-w | Copy to Register        |
| C-c C-b C-y | Insert Register         |
|             |                         |
| M-d         | Delete Line             |
| delete      | Delete Region or Char   |
|             |                         |
| RET         | Newline and Indent      |
| C-j         | Newline                 |
| C-RET       | Open New Line           |
| Tab         | Indent                  |


<a id="reversible-paging-and-scrolling"></a>

## Reversible Paging and Scrolling

-   Paging and scrolling respect relative screen row and absolute column.
-   Paging up and then down again returns point to the same original position.

| Key    | Action                        |
|------ |----------------------------- |
| next   | page-down                     |
| C-v    | page-down                     |
| prior  | page-up                       |
| M-v    | page-up                       |
| M-down | scroll-down                   |
| M-up   | scroll-up                     |
| home   | Beginning of Line/Page/Buffer |
| end    | End of Line/Page/Buffer       |


<a id="temporary-bookmarks"></a>

## Temporary Bookmarks

-   10 bookmarks can be set and navigated between.
-   They can also be moved and deleted.
-   They are temporary in the sense they don't persist between invocations of Emacs.
-   As an extension to Brief, bookmark lines are highlighted in colour. This is [customisable](#customisation).
-   If the package `fringe-helper` is installed, the bookmark number is put in the fringe (which otherwise shows as a tooltip).
-   Bookmarks can be listed & chosen from a menu, [see below](#list-bookmarks). This is also an extension to Brief.
-   Other extensions are a command to allocate the next free bookmark and one to delete all bookmarks.

![img](https://bitbucket.org/MikeWoolley/brf-mode/raw/master/images/bookmarks.png "Bookmarks")

| Key                 | Action                               |
|------------------- |------------------------------------ |
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


<a id="list-bookmarks"></a>

## List Bookmarks

-   Invoke the List Bookmarks menu with `C-c C-b C-l`.
-   This allows you to view and manage all the current bookmarks.

![img](https://bitbucket.org/MikeWoolley/brf-mode/raw/master/images/list-bookmarks.png "List Bookmarks")

| key  | Action                    |
|---- |------------------------- |
| RET  | Jump to bookmark at point |
| SPC  | Jump to bookmark at point |
| d    | Delete bookmark at point  |
| k    | Delete All bookmarks      |
|      |                           |
| down | Move point down           |
| up   | Move point up             |
| <    | Move to start of buffer   |
| >    | Move to end of buffer     |
|      |                           |
| ?    | Help                      |
| h    | Describe Mode             |
| q    | Quit                      |


<a id="cursor-motion-undo"></a>

## Cursor Motion Undo

-   Cursor motion, without any buffer changes, is recorded as an undo-able (& redo-able) action.
-   This works with both built-in Emacs Undo and also with the `Redo.el` & `Redo+.el` packages. I haven't tested it with the plethora of other Undo packages - it should work, but you never know!
-   It is turned off by default (unlike in Brief), but can be enabled by customising option `brf-undo-enable`.

| Key         | Action                                 |
|----------- |-------------------------------------- |
| kp-multiply | Undo                                   |
| M-u         | Undo                                   |
| M-r         | Redo (if `redo` or `redo+` installed). |


<a id="easy-window-management"></a>

## Easy Window Management

-   Create, Switch, Resize and Delete arbitrary windows with simple keystrokes.

| Key                        | Action                        |
|-------------------------- |----------------------------- |
| S- [up, down, right, left] | Switch to Window in Direction |
| f1 [up, down, right, left] | Switch to window in Direction |
| f2 [up, down, right, left] | Resize Window in Direction    |
| M-f2                       | Zoom Window                   |
| f3 [up, down, right, left] | Create Window in Direction    |
| f4 [up, down, right, left] | Delete Window in Direction    |
| C-f4                       | Delete Current Window         |
| S-f4                       | Delete Other Windows          |


## Differences From Brief

![img](https://bitbucket.org/MikeWoolley/brf-mode/raw/master/images/BRIEF-Screenshot.png "Screenshot of the original BRIEF")

-   Inclusive Mark (Alt-m)
    
    "Inclusive" character marking in Brief includes the character under the cursor, whereas in Brf-mode (and Emacs in general) the marked region stops on the character before the cursor. This behaviour is actually Brief's "Non-inclusive Mark" and is the only kind supported in Brf-mode. I don't think it makes any practical difference and so "Inclusive Mark" has not been implemented in Brf-mode.

-   Window Resizing (F2)
    
    When resizing a window in Brief, the user has to hit Enter to end resizing and all other keys are ignored. In Brf-mode, any key or click that is not a cursor key ends resizing, which I personally think is better.


## Known Issues

Please report any issues at the [Brf-mode website bug tracker](https://bitbucket.org/MikeWoolley/brf-mode/issues).

There are a couple of known minor issues:

-   XEmacs Compatibility
    
    Brf-mode no longer works in XEmacs. It's likely to be fairly easy to fix the compatibility issues, but given the demise of XEmacs I don't have any current plans to do this.
    
    Anyone wanting to run Brf-mode on XEmacs should install an older version, as described in [Compatibility](#compatibility).

-   Menu & Toolbar commands for Cut & Paste
    
    Brf-mode replaces the Cut & Paste menu and toolbar commands with versions that respect Line & Column Mode in the same way as the Brf-mode keyboard commands. However Emacs disables the menu and toolbar `Cut` & `Copy` items if there is no marked region, unlike the corresponding Brf-mode keyboard commands.
