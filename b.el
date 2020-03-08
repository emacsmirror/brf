;;; b.el --- Add functionality from the editor Brief -*- lexical-binding: t -*-

;; Copyright (C) 2000-2020 Mike Woolley
;; Author: Mike Woolley <mike@bulsara.com>
;; Package-Version: 1.16
;; Package-Requires: ((fringe-helper "0.1.1") (emacs "24"))
;; Keywords: brief crisp emulations
;; URL: https://bitbucket.org/MikeWoolley/b-mode

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is not an emulation of the old DOS editor `Brief', but
;; rather provides an implementation of specific features that I miss
;; from `Brief'.  Principally these are:
;;
;; * Line-mode cut and paste.
;; * Column-mode cut and paste.
;; * Fully reversible paging and scrolling.
;; * Temporary bookmarks.
;; * Cursor motion undo.
;; * Easy window management.
;;
;; See the README.md file for further details.

;;; Change Log:
;;
;; See the ChangeLog file.

;;; Code:

(require 'b-compat)

(defgroup b nil
  "Add functionality from the editor Brief."
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
  :set (lambda (_symbol value) (b-mode (or value 0)))
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
  "Hook run on entering B mode."
  :type 'hook
  :group 'b)

;;;
;;; Version number
;;;
(defconst b-version "1.16"
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
(require 'b-window)

;;;
;;; Keymap
;;;
(defvar b-mode-map nil
  "Local keymap for B mode.")
(unless b-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Put the B-mode overrides on the same keys as Brief
    (define-key map [(control return)] 'b-insert-line)
    (define-key map "\M-d" 'b-delete-line)
    (define-key map "\M-m" 'set-mark-command)
    (define-key map "\M-l" 'b-mark-line)
    (when (fboundp 'rectangle-mark-mode)
      (define-key map "\M-c" 'rectangle-mark-mode))
    (define-key map [(kp-add)] 'b-copy-region)
    (define-key map [(kp-subtract)] 'b-kill-region)
    (define-key map [(kp-multiply)] 'undo)
    (define-key map "\M-u" 'undo)
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
    (define-key map [(shift up)] 'b-change-window-up)
    (define-key map [(shift down)] 'b-change-window-down)
    (define-key map [(shift left)] 'b-change-window-left)
    (define-key map [(shift right)] 'b-change-window-right)

    ;; Also put them on the original Emacs key-mappings
    (substitute-key-definition 'kill-ring-save 'b-copy-region map (current-global-map))
    (substitute-key-definition 'kill-region 'b-kill-region map (current-global-map))
    (substitute-key-definition 'yank 'b-yank map (current-global-map))
    (substitute-key-definition 'yank-pop 'b-yank-pop map (current-global-map))
    (substitute-key-definition 'beginning-of-line 'b-home map (current-global-map))
    (substitute-key-definition 'end-of-line 'b-end map (current-global-map))

    ;; Function keys
    ;; F1 - Change window
    (let ((f1-prefix (make-sparse-keymap "Point to destination (use cursor keys)")))
      (define-key f1-prefix [(up)] 'b-change-window-up)
      (define-key f1-prefix [(down)] 'b-change-window-down)
      (define-key f1-prefix [(left)] 'b-change-window-left)
      (define-key f1-prefix [(right)] 'b-change-window-right)
      (define-key map [(f1)] f1-prefix))
    ;; F2 - Resize window
    (let ((f2-prefix (make-sparse-keymap "Choose direction to resize (use cursor keys)")))
      (define-key f2-prefix [(up)] 'b-resize-window-up)
      (define-key f2-prefix [(down)] 'b-resize-window-down)
      (define-key f2-prefix [(left)] 'b-resize-window-left)
      (define-key f2-prefix [(right)] 'b-resize-window-right)
      (define-key map [(f2)] f2-prefix))
    ;; F3 - Create window
    (let ((f3-prefix (make-sparse-keymap "Select side for new window (use cursor keys)")))
      (define-key f3-prefix [(up)] 'b-create-window-up)
      (define-key f3-prefix [(down)] 'b-create-window-down)
      (define-key f3-prefix [(left)] 'b-create-window-left)
      (define-key f3-prefix [(right)] 'b-create-window-right)
      (define-key map [(f3)] f3-prefix))
    ;; F4 - Delete window
    (let ((f4-prefix (make-sparse-keymap "Select window to delete (use cursor keys)")))
      (define-key f4-prefix [(up)] 'b-delete-window-up)
      (define-key f4-prefix [(down)] 'b-delete-window-down)
      (define-key f4-prefix [(left)] 'b-delete-window-left)
      (define-key f4-prefix [(right)] 'b-delete-window-right)
      (define-key map [(f4)] f4-prefix))
    ;; These were not original Brief key-sequences
    (define-key map [(control f4)] 'b-delete-current-window)
    (define-key map [(shift f4)] 'delete-other-windows)

    ;; Create new key bindings for my new functions that weren't part of Brief
    (define-key map "\C-c\C-b\C-n" 'b-next-bookmark)
    (define-key map "\C-c\C-b\C-p" 'b-prev-bookmark)
    (define-key map "\C-c\C-b\C-k" 'b-kill-all-bookmarks)
    (define-key map "\C-c\C-b\C-l" 'b-list-bookmarks)
    (define-key map "\C-c\C-b="    'b-allocate-next-available-bookmark)
    (define-key map "\C-c\C-b\C-w" 'b-copy-to-register)
    (define-key map "\C-c\C-b\C-y" 'b-insert-register)

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
(defvar b-prev-truncate-lines nil
  "Previous value of `truncate-lines'.")
(defvar b-prev-scroll-step nil
  "Previous value of `scroll-step'.")
(defvar b-prev-hscroll-margin nil
  "Previous value of `hscroll-margin'.")
(defvar b-prev-hscroll-step nil
  "Previous value of `hscroll-step'.")
(defvar b-prev-scroll-preserve-screen-position nil
  "Previous value of `scroll-preserve-screen-position'.")
(defvar b-prev-c-m nil
  "Previous global binding of CR.")
(defvar b-prev-c-j nil
  "Previous global binding of LF.")

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
	 ;; Force transient-mark-mode and conservative scrolling
	 ;; Remember old settings
	 (setq b-prev-mark-mode (b-transient-mark-mode t))
	 (setq b-prev-truncate-lines (default-value truncate-lines))
	 (setq b-prev-scroll-step scroll-step)
	 (setq b-prev-hscroll-margin hscroll-margin)
	 (setq b-prev-hscroll-step hscroll-step)
	 (setq-default truncate-lines t)
	 (setq scroll-step 1)		  ; Scroll line at a time
	 (setq hscroll-margin 1)	  ; Scroll when we get to the end of the line
	 (setq hscroll-step 1)		  ; Scroll one column at a time

	 ;; Use Emacs scrolling to page up/dn when it supports the new
	 ;; position-preserving behaviour
	 (when (boundp 'scroll-preserve-screen-position)
	   (setq b-prev-scroll-preserve-screen-position scroll-preserve-screen-position)
	   (setq scroll-preserve-screen-position t))

	 ;; Setup return (in the global map) to always indent
	 (setq b-prev-c-m (global-key-binding "\C-m"))
	 (setq b-prev-c-j (global-key-binding "\C-j"))
	 (global-set-key "\C-m" 'newline-and-indent)
	 (global-set-key "\C-j" 'newline)

	 ;; Run mode hook
	 (run-hooks 'b-mode-hook))

	(t ; b-mode off
	 ;; Restore old settings
	 (global-set-key "\C-j" b-prev-c-j)
	 (global-set-key "\C-m" b-prev-c-m)
	 (when (boundp 'scroll-preserve-screen-position)
	   (setq scroll-preserve-screen-position b-prev-scroll-preserve-screen-position))
	 (setq hscroll-step b-prev-hscroll-step)
	 (setq hscroll-margin b-prev-hscroll-margin)
	 (setq scroll-step b-prev-scroll-step)
	 (setq-default truncate-lines b-prev-truncate-lines)
	 (b-transient-mark-mode b-prev-mark-mode))))

;; Add B mode as a minor mode
(add-to-list 'minor-mode-alist '(b-mode b-mode-modeline-string))
(add-to-list 'minor-mode-map-alist (cons 'b-mode b-mode-map))

;; Load successful
(provide 'b)

;;; b.el ends here
