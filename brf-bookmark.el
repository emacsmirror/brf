;;; brf-bookmark.el --- Bookmark feature of brf-mode -*- lexical-binding: t -*-

;; Copyright (C) 2000-2020 Mike Woolley
;; Author: Mike Woolley <mike@bulsara.com>

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

;; See the README.md file for further details.

;;; Code:

(require 'brf-compat)
(eval-when-compile (require 'cl))

(defface brf-bookmark-face '((t (:background "khaki")))
  "Face used to show bookmark."
  :group 'brf)

(defface brf-bookmark-number-face '((t (:foreground "red")))
  "Face used (to set color) of bookmark number."
  :group 'brf)

;;
;; Fringe bitmaps
;;
(defconst brf-fringe-support-flag
  (and (fboundp 'fringe-mode)
       (eval-and-compile (require 'fringe-helper "fringe-helper" t)))
  "Non-nil means this Emacs version has support for programmable fringes.")

(when brf-fringe-support-flag
  (fringe-helper-define 'brf-bookmark-bitmap-0 nil
    "..XXXX.."
    ".XX..XX."
    "XX....XX"
    "XX..X.XX"
    "XX.X..XX"
    "XX....XX"
    ".XX..XX."
    "..XXXX..")

  (fringe-helper-define 'brf-bookmark-bitmap-1 nil
    "...XX..."
    "..XXX..."
    ".X.XX..."
    "...XX..."
    "...XX..."
    "...XX..."
    "...XX..."
    ".XXXXXX.")

  (fringe-helper-define 'brf-bookmark-bitmap-2 nil
    "..XXXX.."
    "XX....XX"
    "......XX"
    "......XX"
    "....XX.."
    "..XX...."
    "XX.....X"
    "XXXXXXXX")

  (fringe-helper-define 'brf-bookmark-bitmap-3 nil
    "..XXXX.."
    "XX....XX"
    "......XX"
    "....XX.."
    "....XX.."
    "......XX"
    "XX....XX"
    "..XXXX..")

  (fringe-helper-define 'brf-bookmark-bitmap-4 nil
    "...XXXX."
    "..XX.XX."
    ".XX..XX."
    "XX...XX."
    "XXXXXXXX"
    ".....XX."
    ".....XX."
    ".....XX.")

  (fringe-helper-define 'brf-bookmark-bitmap-5 nil
    "XXXXXXXX"
    "XX......"
    "XX......"
    ".XXXXX.."
    "......X."
    "......XX"
    "X.....X."
    ".XXXXX..")

  (fringe-helper-define 'brf-bookmark-bitmap-6 nil
    "..XXXX.."
    "XX....XX"
    "XX......"
    "XXXXXX.."
    "XX....XX"
    "XX....XX"
    "XX....XX"
    "..XXXX..")

  (fringe-helper-define 'brf-bookmark-bitmap-7 nil
    "XXXXXXXX"
    "X.....XX"
    "......XX"
    ".....XX."
    "....XX.."
    "...XX..."
    "..XX...."
    ".XX.....")

  (fringe-helper-define 'brf-bookmark-bitmap-8 nil
    ".XXXXXX."
    "XX....XX"
    "XX....XX"
    ".XXXXXX."
    ".XXXXXX."
    "XX....XX"
    "XX....XX"
    ".XXXXXX.")

  (fringe-helper-define 'brf-bookmark-bitmap-9 nil
    "..XXXX.."
    "XX....XX"
    "XX....XX"
    "XX....XX"
    "..XXX.XX"
    "......XX"
    "XX....XX"
    "..XXXX.."))

;; Remove fringe bitmap with:
;; (destroy-fringe-bitmap 'brf-bookmark-bitmap-n)

(defstruct brf-bookmark
  "Bookmark."
  (number nil :read-only t)
  (marker nil :read-only t)
  (overlay nil :read-only t))

(defconst brf-max-bookmarks 10
  "The maximum number of bookmarks.")

(defvar brf-bookmarks (make-vector brf-max-bookmarks nil)
  "Bookmark vector.")

(defvar brf-current-bookmark nil
  "Last bookmark set or jumped to.
This is used as the start point for the next/prev bookmark commands.")

(defun brf-valid-bookmark-number-p (number)
  "Return t if NUMBER is inside the range of valid bookmark numbers."
  (and (>= number 0) (< number brf-max-bookmarks)))

(defun brf-get-bookmark (number)
  "Return bookmark at NUMBER."
  (assert (brf-valid-bookmark-number-p number))
  (let ((bookmark (aref brf-bookmarks number)))
    (assert (or (null bookmark) (= (brf-bookmark-number bookmark) number)))
    bookmark))

(defun brf-valid-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is set, nil otherwise."
  (and bookmark (marker-buffer (brf-bookmark-marker bookmark))))

(defun brf-read-bookmark-number (prompt)
  "Read the bookmark number from the minibuffer as a single character digit.
The user is prompted with PROMPT, which can be nil for no prompt.
This function is meant to be called from a command's interactive form."
  (cl-labels ((digitp (char)
		      (and (>= char ?0) (<= char ?9)))
	      (read-digit (prompt)
			  (do* ((cursor-in-echo-area t)
				(char (ignore-errors (read-char prompt))
				      (ignore-errors (read-char
						      (format "%s(single digit) " prompt)))))
			      ((and char (digitp char)) char)
			    (unless (characterp char)
			      ;; Swallow the offending non-character event which is still pending
			      (read-event))
			    ;; Tell user they've made a mistake
			    (beep))))

    (let ((number (or current-prefix-arg
		      (- (read-digit prompt) ?0))))
      (unless (brf-valid-bookmark-number-p number)
	(user-error "%d is an invalid bookmark number" number))
      (list number))))

(defun brf-make-set-bookmark (number)
  "Generate a command to set bookmark NUMBER at point.
If the command is given a prefix argument, then the bookmark is removed."
  (defalias (intern (format "brf-set-bookmark-%s" (number-to-string number)))
    `(lambda (&optional arg)
       ,(format "Set bookmark %d at point.\nWith ARG, remove the bookmark instead." number)
       (interactive "P")
       (if arg
	   (brf-kill-bookmark ,number)
	 (brf-set-bookmark ,number)))))

(defun brf-set-bookmark (number)
  "Set bookmark NUMBER at point."
  (interactive (brf-read-bookmark-number "Set Bookmark: "))

  ;; Don't allow bookmark to be dropped in the minibuffer
  (when (window-minibuffer-p (selected-window))
    (user-error "Bookmark not allowed in minibuffer"))

  ;; Lookup the bookmark and move it to the new location, create a new one if it doesn't exist yet
  (let ((bookmark (brf-get-bookmark number)))
    (if bookmark
	(brf-move-bookmark bookmark)
      (brf-create-bookmark number)))

  (setq brf-current-bookmark number)
  (message "Bookmark %d dropped" number))

(defun brf-create-bookmark (number)
  "Create new bookmark NUMBER at point."
  (let ((buffer (current-buffer))
	(start-line (brf-bol-position 1))
	(end-line (brf-bol-position 2)))
    (let ((marker (point-marker))
	  (overlay (make-overlay start-line end-line buffer t nil)))
      (set-marker-insertion-type marker t) ; Insert before the marker
      (overlay-put overlay 'face 'brf-bookmark-face)
      (overlay-put overlay 'help-echo (format "Bookmark %d" number))
      (when brf-fringe-support-flag
	(let ((overlay-string (format "%d>" number))
	      (bitmap (intern (format "brf-bookmark-bitmap-%d" number))))
	  (put-text-property 0 (length overlay-string)
			     'display `(left-fringe ,bitmap brf-bookmark-number-face)
			     overlay-string)
	  (overlay-put overlay 'before-string overlay-string)))

      ;; Ensure the bookmark overlay is on the line containing the bookmark
      ;; XEmacs overlay compatibility doesn't support modification hook and barfs
      ;; if this property is set, so don't do this if XEmacs
      (unless brf-xemacs-flag
	(let ((protect-overlay
	       (lambda (overlay after _begin _end &optional _len)
		 "Ensure the bookmark overlay is on the line containing the bookmark."
		 (when after
		   (save-excursion
		     (goto-char marker)
		     (move-overlay overlay (brf-bol-position 1) (brf-bol-position 2)))))))
	  (overlay-put overlay 'modification-hooks (list protect-overlay))
	  (overlay-put overlay 'insert-in-front-hooks (list protect-overlay))))

      ;; Add the new bookmark to the vector
      (setf (aref brf-bookmarks number)
	    (make-brf-bookmark :number number :marker marker :overlay overlay)))))

(defun brf-allocate-next-available-bookmark ()
  "Allocate the next available bookmark."
  (interactive)
  (let ((number 0))
    (while (and (brf-valid-bookmark-number-p number)
		(brf-valid-bookmark-p (brf-get-bookmark number)))
      (incf number))
    (if (brf-valid-bookmark-number-p number)
	(brf-set-bookmark number)
      (user-error "All bookmarks allocated"))))

(defun brf-move-bookmark (bookmark)
  "Move BOOKMARK to point."
  (let ((buffer (current-buffer))
	(overlay (brf-bookmark-overlay bookmark))
	(start-line (brf-bol-position 1))
 	(end-line (brf-bol-position 2)))
    (move-marker (brf-bookmark-marker bookmark) (point) buffer)
    (move-overlay overlay start-line end-line buffer)))

(defun brf-kill-bookmark (number)
  "Kill bookmark NUMBER."
  (interactive (brf-read-bookmark-number "Kill Bookmark: "))
  (let ((bookmark (brf-get-bookmark number)))
    (unless (brf-valid-bookmark-p bookmark)
      (user-error "Bookmark %d is not set" number))
    (move-marker (brf-bookmark-marker bookmark) nil)
    (delete-overlay (brf-bookmark-overlay bookmark))))

(defun brf-kill-all-bookmarks ()
  "Kill all bookmarks."
  (interactive)
  (dotimes (number brf-max-bookmarks)
    (let ((bookmark (brf-get-bookmark number)))
      (when (brf-valid-bookmark-p bookmark)
	(brf-kill-bookmark number)))))

(defun brf-jump-to-bookmark (number)
  "Jump to bookmark NUMBER."
  ;; Read the bookmark number
  (interactive (brf-read-bookmark-number "Jump to Bookmark: "))

  ;; Lookup the bookmark
  (let ((bookmark (brf-get-bookmark number)))
    (unless (brf-valid-bookmark-p bookmark)
      (user-error "Bookmark %d is not set" number))
    (let ((marker (brf-bookmark-marker bookmark)))
      (switch-to-buffer (marker-buffer marker))
      (goto-char marker)))
  (setq brf-current-bookmark number))

(defun brf-next-bookmark (&optional arg)
  "Jump to the next bookmark.
With ARG jump to the previous one."
  (interactive "P")
  (block brf-next-bookmark	  	; Annoying that this is necessary...
    (when brf-current-bookmark
      ;; Work out if we're going forwards or backwards through the bookmarks
      (let ((dir-fn (if arg #'- #'+)))
	;; Find the next bookmark in that direction
	(dotimes (i brf-max-bookmarks)	; `dotimes' lexical-binding bug stops me using the result form now
	  (let* ((number (mod (funcall dir-fn brf-current-bookmark i 1) brf-max-bookmarks))
		 (bookmark (brf-get-bookmark number)))
	    (when (brf-valid-bookmark-p bookmark)
	      (brf-jump-to-bookmark number)
	      (return-from brf-next-bookmark))))))
    (user-error "No bookmarks have been set")))

(defun brf-prev-bookmark (&optional arg)
  "Jump to the previous bookmark.
With ARG jump to the next one."
  (interactive "P")
  (brf-next-bookmark (null arg)))

(defun brf-list-bookmarks ()
  "Show list of all bookmarks."
  (interactive)
  (unless brf-current-bookmark
    (user-error "No bookmarks have been set"))

  ;; List selection buffer is provided by `generic-menu'
  ;; *** Mike: Fix Me ***: Replace this with something more widely available (or my own code)
  (if (require 'generic-menu "generic-menu" t)
      (let ((map (make-sparse-keymap)))
	(require 'derived) ; `generic-menu' should really be doing this...

	;; Add some command to delete bookmarks
	(define-key map "d" 'brf-bookmark-menu-kill)
	(define-key map "k" 'brf-bookmark-menu-kill-all)
	(define-key map "?" nil) ; Disable `generic-mode'-specific help

	;; Show the Bookmark menu
	(gm-popup :buffer-name "*Brf Bookmarks*"
		  :mode-name "Bookmarks"
		  :header-line "Bookmarks: [SELECT] Jump to bookmark, [d] Delete, [k] Delete All, [q] Quit."
		  :max-entries brf-max-bookmarks
		  :truncate-lines t
		  :regexp-start-position (format "^[* ][ \t]+%d" brf-current-bookmark)
		  :elements (loop for idx from 0 to (1- brf-max-bookmarks)
				  collect idx)
		  :keymap map
		  :font-lock-keywords #'brf-bookmark-menu-font-lock
		  :select-callback #'brf-bookmark-menu-select
		  :display-string-function #'brf-bookmark-menu-display)

	;; Size the window to show all the bookmarks, if possible
	(fit-window-to-buffer))

    ;; `generic-menu' not available
    ;; Silence the byte-compiler, about the missing gm- functions
    (declare-function gm-popup "ext:generic-menu" (&rest plain-properties))
    (declare-function gm-quit "ext:generic-menu" nil)
    (declare-function gm-full-refresh "ext:generic-menu" nil)
    (user-error "Please install generic-menu")))

(defun brf-bookmark-menu-font-lock ()
  "Return font-lock keywords to fontify the menu buffer."
  '(("\\<[0-9]\\>" . font-lock-constant-face)	; Bookmark #
    ("\\<NOT SET\\>" . compilation-error-face)	; "NOT SET"
    ("^\\*" . font-lock-function-name-face)	; Current bookmark
    ("%\\b\\(.*\\)$" . 1)))			; Buffer name

(defun brf-bookmark-menu-select (idx)
  "Jump to bookmark at menu IDX."
  (let ((bookmark (brf-get-bookmark idx)))
    (cond ((brf-valid-bookmark-p bookmark)
	   (gm-quit)
	   (brf-jump-to-bookmark idx))
	  (t ; Bookmark not set
	   (user-error "Bookmark %d is not set" idx)))))

(defun brf-bookmark-menu-display (idx)
  "Return display string for bookmark at menu IDX."
  (let ((bookmark (brf-get-bookmark idx)))
    (cond ((brf-valid-bookmark-p bookmark)
	   (let ((marker (brf-bookmark-marker bookmark)))
	     (with-current-buffer (marker-buffer marker)
	       (save-excursion
		 (goto-char marker)
		 (format "%s %d\tL%d\tC%d\t%d%%\t%s"
			 (if (= brf-current-bookmark idx) "*" " ")
			 idx
			 (brf-current-line) (brf-current-column)
			 (/ (* (point) 100) (max (buffer-size) 1))
			 (buffer-name))))))
	  (t ; Bookmark not set
	   (format "  %d NOT SET" idx)))))

(defun brf-bookmark-menu-kill ()
  "Kill the current bookmark when in the bookmark menu."
  (interactive)
  (let ((line (count-lines (point-min) (brf-bol-position))))
    (if (or (< line 1)
	    (> line brf-max-bookmarks))
	(user-error "You aren't on a bookmark line")
      (brf-kill-bookmark (1- line))
      (gm-full-refresh))))

(defun brf-bookmark-menu-kill-all ()
  "Kill all bookmarks when in the bookmark menu."
  (interactive)
  (brf-kill-all-bookmarks)
  (gm-full-refresh))

(defun brf-current-line ()
  "Return current line number of point (starting at 1)."
  (+ (count-lines 1 (point))
     (if (= (current-column) 0) 1 0)))

(defun brf-current-column ()
  "Return current column of point (starting at 1)."
  (1+ (current-column)))

(provide 'brf-bookmark)

;; Local Variables:
;; tab-width: 8
;; indent-tabs-mode: t
;; End:

;;; brf-bookmark.el ends here
