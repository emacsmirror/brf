;;; brf.el --- Add functionality from the editor Brief -*- lexical-binding: t -*-

;; Copyright (C) 2000-2020 Mike Woolley
;; Author: Mike Woolley <mike@bulsara.com>
;; Package-Version: 1.17
;; Package-Requires: ((fringe-helper "0.1.1") (emacs "24"))
;; Keywords: brief crisp emulations
;; URL: https://bitbucket.org/MikeWoolley/brf-mode

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
;; See README.md or Info manual for further details.

;;; Change Log:
;;
;; See the ChangeLog file.

;;; Code:

(require 'brf-compat)

(defgroup brf nil
  "Add functionality from the editor Brief."
  :prefix "brf-"
  :group 'editing)

(defcustom brf-mode-modeline-string " Brf"
  "String to display in the mode-line when Brf mode is enabled.
Set this to nil to conserve valuable mode line space."
  :type 'string
  :group 'brf)

;;;
;;; Version number
;;;
(defconst brf-version "1.17"
  "Version number of Brf mode.")

(defun brf-version ()
  "Version number of Brf mode."
  (interactive)
  (message "Brf version %s" brf-version))

;;;
;;; Load the different features
;;;
(require 'brf-bookmark)
(require 'brf-editing)
(require 'brf-marking)
(require 'brf-movement)
(require 'brf-undo)
(require 'brf-window)

;;;
;;; Keymap
;;;
(defvar brf-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Put the Brf mode overrides on the same keys as Brief
    (define-key map [(control return)] 'brf-insert-line)
    (define-key map "\M-d" 'brf-delete-line)
    (define-key map "\M-m" 'set-mark-command)
    (define-key map "\M-l" 'brf-mark-line)
    (when (fboundp 'rectangle-mark-mode)
      (define-key map "\M-c" 'rectangle-mark-mode))
    (define-key map [(kp-add)] 'brf-copy-region)
    (define-key map [(kp-subtract)] 'brf-kill-region)
    (define-key map [(kp-multiply)] 'undo)
    (define-key map "\M-u" 'undo)
    (define-key map [(delete)] 'brf-delete)
    (define-key map [(kp-delete)] 'brf-delete)
    (define-key map [(meta up)] 'brf-row-up)
    (define-key map [(meta down)] 'brf-row-down)
    (define-key map [(home)] 'brf-home)
    (define-key map [(end)] 'brf-end)
    (define-key map "\t" 'brf-tab)
    (define-key map "\M-=" 'brf-next-bookmark)
    (define-key map [(meta kp-add)] 'brf-next-bookmark)
    (define-key map "\M--" 'brf-prev-bookmark)
    (define-key map [(meta kp-subtract)] 'brf-prev-bookmark)
    (define-key map [(shift up)] 'brf-change-window-up)
    (define-key map [(shift down)] 'brf-change-window-down)
    (define-key map [(shift left)] 'brf-change-window-left)
    (define-key map [(shift right)] 'brf-change-window-right)

    ;; Also put them on the original Emacs key-mappings
    (substitute-key-definition 'kill-ring-save 'brf-copy-region map (current-global-map))
    (substitute-key-definition 'kill-region 'brf-kill-region map (current-global-map))
    (substitute-key-definition 'yank 'brf-yank map (current-global-map))
    (substitute-key-definition 'yank-pop 'brf-yank-pop map (current-global-map))
    (substitute-key-definition 'beginning-of-line 'brf-home map (current-global-map))
    (substitute-key-definition 'end-of-line 'brf-end map (current-global-map))

    ;; Function keys
    ;; F1 - Change window
    (let ((f1-prefix (make-sparse-keymap "Point to destination (use cursor keys)")))
      (define-key f1-prefix [(up)] 'brf-change-window-up)
      (define-key f1-prefix [(down)] 'brf-change-window-down)
      (define-key f1-prefix [(left)] 'brf-change-window-left)
      (define-key f1-prefix [(right)] 'brf-change-window-right)
      (define-key map [(f1)] f1-prefix))
    ;; F2 - Resize window
    (let ((f2-prefix (make-sparse-keymap "Choose direction to resize (use cursor keys)")))
      (define-key f2-prefix [(up)] 'brf-resize-window-up)
      (define-key f2-prefix [(down)] 'brf-resize-window-down)
      (define-key f2-prefix [(left)] 'brf-resize-window-left)
      (define-key f2-prefix [(right)] 'brf-resize-window-right)
      (define-key map [(f2)] f2-prefix))
    ;; F3 - Create window
    (let ((f3-prefix (make-sparse-keymap "Select side for new window (use cursor keys)")))
      (define-key f3-prefix [(up)] 'brf-create-window-up)
      (define-key f3-prefix [(down)] 'brf-create-window-down)
      (define-key f3-prefix [(left)] 'brf-create-window-left)
      (define-key f3-prefix [(right)] 'brf-create-window-right)
      (define-key map [(f3)] f3-prefix))
    ;; F4 - Delete window
    (let ((f4-prefix (make-sparse-keymap "Select window to delete (use cursor keys)")))
      (define-key f4-prefix [(up)] 'brf-delete-window-up)
      (define-key f4-prefix [(down)] 'brf-delete-window-down)
      (define-key f4-prefix [(left)] 'brf-delete-window-left)
      (define-key f4-prefix [(right)] 'brf-delete-window-right)
      (define-key map [(f4)] f4-prefix))
    ;; These were not original Brief key-sequences
    (define-key map [(control f4)] 'brf-delete-current-window)
    (define-key map [(shift f4)] 'delete-other-windows)

    ;; Create new key bindings for my new functions that weren't part of Brief
    (define-key map "\C-c\C-b\C-n" 'brf-next-bookmark)
    (define-key map "\C-c\C-b\C-p" 'brf-prev-bookmark)
    (define-key map "\C-c\C-b\C-k" 'brf-kill-all-bookmarks)
    (define-key map "\C-c\C-b\C-l" 'brf-list-bookmarks)
    (define-key map "\C-c\C-b="    'brf-allocate-next-available-bookmark)
    (define-key map "\C-c\C-b\C-w" 'brf-copy-to-register)
    (define-key map "\C-c\C-b\C-y" 'brf-insert-register)

    ;; Try and find the existing commands for scrolling up/down,
    ;; as these are different in Emacs & XEmacs
    (let ((scroll-up-cmd (global-key-binding [(next)]))
	  (scroll-down-cmd (global-key-binding [(prior)])))
      (when scroll-up-cmd
	(substitute-key-definition scroll-up-cmd 'brf-page-down map (current-global-map)))
      (when scroll-down-cmd
	(substitute-key-definition scroll-down-cmd 'brf-page-up map (current-global-map))))

    ;; Setup the bookmarks on the M-digit keys, like in Brief
    ;; Prefix args will have to be entered with the C-digit or C-U number methods...
    (dotimes (digit brf-max-bookmarks)
      (define-key map (vector (list 'meta (+ digit ?0))) (brf-make-set-bookmark digit)))
    (define-key map "\M-j" 'brf-jump-to-bookmark)

    map)
  "Local keymap for Brf mode.")

;;;
;;; Brf minor mode
;;;
(defvar brf-prev-mark-mode nil
  "Previous value of transient mark mode.")
(defvar brf-prev-truncate-lines nil
  "Previous value of `truncate-lines'.")
(defvar brf-prev-scroll-step nil
  "Previous value of `scroll-step'.")
(defvar brf-prev-hscroll-margin nil
  "Previous value of `hscroll-margin'.")
(defvar brf-prev-hscroll-step nil
  "Previous value of `hscroll-step'.")
(defvar brf-prev-scroll-preserve-screen-position nil
  "Previous value of `scroll-preserve-screen-position'.")
(defvar brf-prev-c-m nil
  "Previous global binding of CR.")
(defvar brf-prev-c-j nil
  "Previous global binding of LF.")

;;;###autoload
(define-minor-mode brf-mode
  nil ; Use default doc string
  :lighter brf-mode-modeline-string
  :keymap brf-mode-map
  :global t

  ;; Processing that needs to be done when the mode is started or stopped
  (cond (brf-mode
	 ;; Force transient-mark-mode and conservative scrolling
	 ;; Remember old settings
	 (setq brf-prev-mark-mode (brf-transient-mark-mode t))
	 (setq brf-prev-truncate-lines (default-value truncate-lines))
	 (setq brf-prev-scroll-step scroll-step)
	 (setq brf-prev-hscroll-margin hscroll-margin)
	 (setq brf-prev-hscroll-step hscroll-step)
	 (setq-default truncate-lines t)
	 (setq scroll-step 1)		  ; Scroll line at a time
	 (setq hscroll-margin 1)	  ; Scroll when we get to the end of the line
	 (setq hscroll-step 1)		  ; Scroll one column at a time

	 ;; Use Emacs scrolling to page up/dn when it supports the new
	 ;; position-preserving behaviour
	 (when (boundp 'scroll-preserve-screen-position)
	   (setq brf-prev-scroll-preserve-screen-position scroll-preserve-screen-position)
	   (setq scroll-preserve-screen-position t))

	 ;; Setup return (in the global map) to always indent
	 (setq brf-prev-c-m (global-key-binding "\C-m"))
	 (setq brf-prev-c-j (global-key-binding "\C-j"))
	 (global-set-key "\C-m" 'newline-and-indent)
	 (global-set-key "\C-j" 'newline))

	(t ; brf-mode off
	 ;; Restore old settings
	 (global-set-key "\C-j" brf-prev-c-j)
	 (global-set-key "\C-m" brf-prev-c-m)
	 (when (boundp 'scroll-preserve-screen-position)
	   (setq scroll-preserve-screen-position brf-prev-scroll-preserve-screen-position))
	 (setq hscroll-step brf-prev-hscroll-step)
	 (setq hscroll-margin brf-prev-hscroll-margin)
	 (setq scroll-step brf-prev-scroll-step)
	 (setq-default truncate-lines brf-prev-truncate-lines)
	 (brf-transient-mark-mode brf-prev-mark-mode))))

(provide 'brf)

;; Local Variables:
;; tab-width: 8
;; indent-tabs-mode: t
;; End:

;;; brf.el ends here
