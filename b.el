;;; b.el --- Brief editor emulator

;; Copyright (C) 2002 Mike Woolley
;; Author: Mike Woolley <mike@bulsara.com>
;; Version: $Header: /Users/mike/Dev/cvsrep/emacs/emacs/b/b.el,v 1.2 2009/10/21 16:07:11 Mike Exp $
;; Keywords: convenience emulations

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;  This package provides an implementation of the some features that
;;  I miss from the old DOS editor `Brief'. Principally, these are:
;;
;;  * Line-mode cut and paste.
;;  * Column-mode cut and paste (not implemented yet).
;;  * Decent paging and scrolling.
;;  * Temporary bookmarks.
;;  * Cursor motion undo (not fully working yet).
;;
;;  However, the functions have been implemented in an Emacs-style
;;  way, respond to prefix args and where they override Emacs
;;  functions live on the Emacs key bindings etc.
;;
;;  The code has been tested on Emacs 20, Emacs 21 pretest
;;  and XEmacs 21.1 & 21.2.

;;; Change Log:
;;
;;  Version 1.08 2002-01-08 Mike Woolley <mike@bulsara.com>
;;  * Split into separate files for ease of working.
;;
;;  Version 1.07 2001-08-12 Mike Woolley <mike@bulsara.com>
;;  * Lots of small changes and bug fixes.
;;
;;  Version 1.06 2001-08-02 Mike Woolley <mike@bulsara.com>
;;  * Renamed to b-mode, due to the large number of `brief.el's out
;;  there and particularly because this mode is not really an emulation
;;  of Brief, more a homage to Brief in Emacs.
;;  * Added new commands to cycle backwards and forwards through the
;;  bookmarks and to list them.
;;  * Added new prefix key \C-c\c-b for infrequently used commands for
;;  this mode.
;;
;;  Version 1.05 2001-05-21 Mike Woolley <mike@bulsara.com>
;;  * Fixed some minor problems in the bookmark code.
;;  * Now displays the bookmark number in the overlay.
;;  * Turned `brief-protect-overlay' into a closure.
;;  * Add command to remove bookmarks.
;;
;;  Version 1.04 2001-03-12 Mike Woolley <mike@bulsara.com>
;;  * Added bookmarks.
;;  * Moved the XEmacs specific code into functions.
;;  * Removed the byte compiler warnings.
;;
;;  Version 1.03 2001-02-22 Mike Woolley <mike@bulsara.com>
;;  * Added tab key handling.
;;  * newline-and-indent setup in global map.
;;  * Tidied up doc strings.
;;
;;  Version 1.02 2001-02-15 Mike Woolley <mike@bulsara.com>
;;  * Changed M-d to delete a line rather than kill a line.
;;
;;  Version 1.01 - Mike Woolley <mike@bulsara.com>
;;  * Added Brief-style Home and End key handling
;;
;;  Version 1.00 - Mike Woolley <mike@bulsara.com>
;;  * Initial version.
;;  * Cursor motion undo not working yet.
;;

;;; Code:

(require 'b-compat)

(defgroup b nil
  "Emulator for Brief."
  :prefix "b-"
  :group 'editing)

;;;###autoload
(defcustom b-mode nil
  "Track status of B mode.
A value of t indicates B mode is enabled.
A value of nil means B mode is not enabled.

Setting this variable directly does not take effect;
use either \\[execute-extended-command] customize or the function `b-mode'."
  :type 'boolean
  :set (lambda (symbol value) (b-mode (or value 0)))
  :initialize 'custom-initialize-default
  :require 'b
  :version "20.4"
  :group 'b)

(defcustom b-mode-modeline-string " B"
  "String to display in the mode-line when B mode is enabled.
Set this to nil to conserve valuable mode line space."
  :type 'string
  :group 'b)

(defcustom b-mode-hook nil
  "Hook run on entering B mode"
  :type 'hook
  :group 'b)

;;;
;;; Version number
;;;
(defconst b-version "1.08"
  "Version number of B mode.")

(defun b-version ()
  "Version number of B mode."
  (interactive)
  (message "B version %s" b-version))

;;;
;;; Load the different features
;;;
(require 'b-bookmark)
(require 'b-editing)
(require 'b-marking)
(require 'b-movement)
(require 'b-undo)

;;;
;;; Keymap
;;;
(defvar b-mode-map nil
  "Local keymap for B mode.")
(unless b-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Put our definitions on the same keys as Brief
    (define-key map [(control return)] 'b-insert-line)
    (define-key map "\M-d" 'b-delete-line)
    (define-key map "\M-l" 'b-mark-line)
    (define-key map [(kp-add)] 'b-copy-region)
    (define-key map [(kp-subtract)] 'b-kill-region)
    (define-key map [(kp-multiply)] 'undo)
    (define-key map [(delete)] 'b-delete)
    (define-key map [(kp-delete)] 'b-delete)
    (define-key map [(meta up)] 'b-row-up)
    (define-key map [(meta down)] 'b-row-down)
    (define-key map [(home)] 'b-home)
    (define-key map [(end)] 'b-end)
    (define-key map "\t" 'b-tab)
    (define-key map "\M-=" 'b-next-bookmark)
    (define-key map [(meta kp-add)] 'b-next-bookmark)
    (define-key map "\M--" 'b-prev-bookmark)
    (define-key map [(meta kp-subtract)] 'b-prev-bookmark)

    ;; Also put them on the Emacs keys
    (substitute-key-definition 'kill-ring-save 'b-copy-region map (current-global-map))
    (substitute-key-definition 'kill-region 'b-kill-region map (current-global-map))
    (substitute-key-definition 'yank 'b-yank map (current-global-map))
    (substitute-key-definition 'yank-pop 'b-yank-pop map (current-global-map))
    (substitute-key-definition 'beginning-of-line 'b-home map (current-global-map))
    (substitute-key-definition 'end-of-line 'b-end map (current-global-map))

    ;; Create new key bindings for my new functions that weren't part of Brief
    (define-key map "\C-c\C-b\C-n" 'b-next-bookmark)
    (define-key map "\C-c\C-b\C-p" 'b-prev-bookmark)
    (define-key map "\C-c\C-b\C-k" 'b-kill-all-bookmarks)
    (define-key map "\C-c\C-b\C-l" 'b-list-bookmarks)

    ;; Try and find the existing commands for scrolling up/down,
    ;; as these are different in Emacs & XEmacs
    (let ((scroll-up-cmd (global-key-binding [(next)]))
	  (scroll-down-cmd (global-key-binding [(prior)])))
      (when scroll-up-cmd
	(substitute-key-definition scroll-up-cmd 'b-page-down map (current-global-map)))
      (when scroll-down-cmd
	(substitute-key-definition scroll-down-cmd 'b-page-up map (current-global-map))))

    ;; Setup the bookmarks on the M-digit keys, like in Brief
    ;; Prefix args will have to be entered with the C-digit or C-U number methods...
    (dotimes (digit b-max-bookmarks)
      (define-key map (vector (list 'meta (+ digit ?0))) (b-make-set-bookmark digit)))
    (define-key map "\M-j" 'b-jump-to-bookmark)

    (setq b-mode-map map)))

;;;
;;; B minor mode
;;;
(defvar b-prev-mark-mode nil
  "Previous value of transient mark mode.")
(defvar b-prev-c-m nil
  "Previous global binding of C-m.")
(defvar b-prev-c-j nil
  "Previous global binding of C-j.")

;;;###autoload
(defun b-mode (&optional arg)
  "Toggle B minor mode.
With ARG, turn B mode on if ARG is positive, off otherwise.

Key bindings:
\\{b-mode-map}"
  (interactive "P")

  ;; Turn the mode on or off
  (setq b-mode
	(if (null arg)
	    (not b-mode)
	  (> (prefix-numeric-value arg) 0)))

  ;; Processing that needs to be done when the mode is started or stopped
  (cond (b-mode
	 ;; Force transient-mark-mode, remember old setting
	 (setq b-prev-mark-mode (b-transient-mark-mode t))
	 ;; Setup return (in the global map) to always indent
	 (setq b-prev-c-m (global-key-binding "\C-m"))
	 (setq b-prev-c-j (global-key-binding "\C-j"))
	 (global-set-key "\C-m" 'newline-and-indent)
	 (global-set-key "\C-j" 'newline)
	 ;; Run mode hook
	 (run-hooks 'b-mode-hook))
	(t				; b-mode off
	 ;; Restore old settings
	 (global-set-key "\C-m" b-prev-c-m)
	 (global-set-key "\C-j" b-prev-c-j)
	 (b-transient-mark-mode b-prev-mark-mode))))

;; Add B mode as a minor mode
(add-to-list 'minor-mode-alist '(b-mode b-mode-modeline-string))
(add-to-list 'minor-mode-map-alist (cons 'b-mode b-mode-map))

;; Load successful
(provide 'b)

;;; b.el ends here
