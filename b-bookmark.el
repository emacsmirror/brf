;;; b-bookmark.el --- Bookmark feature of b-mode -*- lexical-binding: t -*-

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

(require 'b-compat)
(eval-when-compile (require 'cl))

(defface b-bookmark-face '((t (:background "khaki")))
  "Face used to show bookmark."
  :group 'b)

(defface b-bookmark-number-face '((t (:foreground "red")))
  "Face used (to set color) of bookmark number."
  :group 'b)

;;
;; Fringe bitmaps
;;
(defconst b-fringe-support-flag
	(and (fboundp 'fringe-mode)
	     (eval-and-compile (require 'fringe-helper "fringe-helper" t)))
	"Non-nil means this Emacs version has support for programmable fringes.")

(when b-fringe-support-flag
  (fringe-helper-define 'b-bookmark-bitmap-0 nil
    "..XXXX.."
    ".XX..XX."
    "XX....XX"
    "XX..X.XX"
    "XX.X..XX"
    "XX....XX"
    ".XX..XX."
    "..XXXX..")

  (fringe-helper-define 'b-bookmark-bitmap-1 nil
    "...XX..."
    "..XXX..."
    ".X.XX..."
    "...XX..."
    "...XX..."
    "...XX..."
    "...XX..."
    ".XXXXXX.")

  (fringe-helper-define 'b-bookmark-bitmap-2 nil
    "..XXXX.."
    "XX....XX"
    "......XX"
    "......XX"
    "....XX.."
    "..XX...."
    "XX.....X"
    "XXXXXXXX")

  (fringe-helper-define 'b-bookmark-bitmap-3 nil
    "..XXXX.."
    "XX....XX"
    "......XX"
    "....XX.."
    "....XX.."
    "......XX"
    "XX....XX"
    "..XXXX..")

  (fringe-helper-define 'b-bookmark-bitmap-4 nil
    "...XXXX."
    "..XX.XX."
    ".XX..XX."
    "XX...XX."
    "XXXXXXXX"
    ".....XX."
    ".....XX."
    ".....XX.")

  (fringe-helper-define 'b-bookmark-bitmap-5 nil
    "XXXXXXXX"
    "XX......"
    "XX......"
    ".XXXXX.."
    "......X."
    "......XX"
    "X.....X."
    ".XXXXX..")

  (fringe-helper-define 'b-bookmark-bitmap-6 nil
    "..XXXX.."
    "XX....XX"
    "XX......"
    "XXXXXX.."
    "XX....XX"
    "XX....XX"
    "XX....XX"
    "..XXXX..")

  (fringe-helper-define 'b-bookmark-bitmap-7 nil
    "XXXXXXXX"
    "X.....XX"
    "......XX"
    ".....XX."
    "....XX.."
    "...XX..."
    "..XX...."
    ".XX.....")

  (fringe-helper-define 'b-bookmark-bitmap-8 nil
    ".XXXXXX."
    "XX....XX"
    "XX....XX"
    ".XXXXXX."
    ".XXXXXX."
    "XX....XX"
    "XX....XX"
    ".XXXXXX.")

  (fringe-helper-define 'b-bookmark-bitmap-9 nil
    "..XXXX.."
    "XX....XX"
    "XX....XX"
    "XX....XX"
    "..XXX.XX"
    "......XX"
    "XX....XX"
    "..XXXX.."))

;; Remove fringe bitmap with:
;; (destroy-fringe-bitmap 'b-bookmark-bitmap-n)

(defstruct b-bookmark
  "Bookmark."
  (number nil :read-only t)
  (marker nil :read-only t)
  (overlay nil :read-only t))

(defconst b-max-bookmarks 10
  "The maximum number of bookmarks.")

(defvar b-bookmarks (make-vector b-max-bookmarks nil)
  "Bookmark vector.")

(defvar b-current-bookmark nil
  "Last bookmark set or jumped to.
This is used as the start point for the next/prev bookmark commands.")

(defun b-valid-bookmark-number-p (number)
  "Return t if NUMBER is inside the range of valid bookmark numbers."
  (and (>= number 0) (< number b-max-bookmarks)))

(defun b-get-bookmark (number)
  "Return bookmark at NUMBER."
  (assert (b-valid-bookmark-number-p number))
  (let ((bookmark (aref b-bookmarks number)))
    (assert (or (null bookmark) (= (b-bookmark-number bookmark) number)))
    bookmark))

(defun b-valid-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is set, nil otherwise."
  (and bookmark (marker-buffer (b-bookmark-marker bookmark))))

(defun b-read-bookmark-number (prompt)
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
      (unless (b-valid-bookmark-number-p number)
	(user-error (format "%d is an invalid bookmark number" number)))
      (list number))))

(defun b-make-set-bookmark (number)
  "Generate a command to set bookmark NUMBER at point.
If the command is given a prefix argument, then the bookmark is removed."
  (defalias (intern (format "b-set-bookmark-%s" (number-to-string number)))
    `(lambda (&optional arg)
       ,(format "Set bookmark %d at point.\nWith ARG, remove the bookmark instead." number)
       (interactive "P")
       (if arg
	   (b-kill-bookmark ,number)
	 (b-set-bookmark ,number)))))

(defun b-set-bookmark (number)
  "Set bookmark NUMBER at point."
  (interactive (b-read-bookmark-number "Set Bookmark: "))

  ;; Don't allow bookmark to be dropped in the minibuffer
  (when (window-minibuffer-p (selected-window))
    (user-error "Bookmark not allowed in minibuffer"))

  ;; Lookup the bookmark and move it to the new location, create a new one if it doesn't exist yet
  (let ((bookmark (b-get-bookmark number)))
    (if bookmark
	(b-move-bookmark bookmark)
      (b-create-bookmark number)))

  (setq b-current-bookmark number)
  (message "Bookmark %d dropped" number))

(defun b-create-bookmark (number)
  "Create new bookmark NUMBER at point."
  (let ((buffer (current-buffer))
	(start-line (b-bol-position 1))
	(end-line (b-bol-position 2)))
    (let ((marker (point-marker))
	  (overlay (make-overlay start-line end-line buffer t nil)))
      (set-marker-insertion-type marker t) ; Insert before the marker
      (overlay-put overlay 'face 'b-bookmark-face)
      (overlay-put overlay 'help-echo (format "Bookmark %d" number))
      (when b-fringe-support-flag
	(let ((overlay-string (format "%d>" number))
	      (bitmap (intern (format "b-bookmark-bitmap-%d" number))))
	  (put-text-property 0 (length overlay-string)
			     'display `(left-fringe ,bitmap b-bookmark-number-face)
			     overlay-string)
	  (overlay-put overlay 'before-string overlay-string)))

      ;; Ensure the bookmark overlay is on the line containing the bookmark
      ;; XEmacs overlay compatibility doesn't support modification hook and barfs
      ;; if this property is set, so don't do this if XEmacs
      (unless b-xemacs-flag
	(let ((protect-overlay
	       (lambda (overlay after _begin _end &optional _len)
		 "Ensure the bookmark overlay is on the line containing the bookmark."
		 (when after
		   (save-excursion
		     (goto-char marker)
		     (move-overlay overlay (b-bol-position 1) (b-bol-position 2)))))))
	  (overlay-put overlay 'modification-hooks (list protect-overlay))
	  (overlay-put overlay 'insert-in-front-hooks (list protect-overlay))))

      ;; Add the new bookmark to the vector
      (setf (aref b-bookmarks number)
	    (make-b-bookmark :number number :marker marker :overlay overlay)))))

(defun b-allocate-next-available-bookmark ()
  "Allocate the next available bookmark."
  (interactive)
  (let ((number 0))
    (while (and (b-valid-bookmark-number-p number)
		(b-valid-bookmark-p (b-get-bookmark number)))
      (incf number))
    (if (b-valid-bookmark-number-p number)
	(b-set-bookmark number)
      (user-error "All bookmarks allocated"))))

(defun b-move-bookmark (bookmark)
  "Move BOOKMARK to point."
  (let ((buffer (current-buffer))
	(overlay (b-bookmark-overlay bookmark))
	(start-line (b-bol-position 1))
 	(end-line (b-bol-position 2)))
    (move-marker (b-bookmark-marker bookmark) (point) buffer)
    (move-overlay overlay start-line end-line buffer)))

(defun b-kill-bookmark (number)
  "Kill bookmark NUMBER."
  (interactive (b-read-bookmark-number "Kill Bookmark: "))
  (let ((bookmark (b-get-bookmark number)))
    (unless (b-valid-bookmark-p bookmark)
      (user-error (format "Bookmark %d is not set" number)))
    (move-marker (b-bookmark-marker bookmark) nil)
    (delete-overlay (b-bookmark-overlay bookmark))))

(defun b-kill-all-bookmarks ()
  "Kill all bookmarks."
  (interactive)
  (dotimes (number b-max-bookmarks)
    (let ((bookmark (b-get-bookmark number)))
      (when (b-valid-bookmark-p bookmark)
	(b-kill-bookmark number)))))

(defun b-jump-to-bookmark (number)
  "Jump to bookmark NUMBER."
  ;; Read the bookmark number
  (interactive (b-read-bookmark-number "Jump to Bookmark: "))

  ;; Lookup the bookmark
  (let ((bookmark (b-get-bookmark number)))
    (unless (b-valid-bookmark-p bookmark)
      (user-error (format "Bookmark %d is not set" number)))
    (let ((marker (b-bookmark-marker bookmark)))
      (switch-to-buffer (marker-buffer marker))
      (goto-char marker)))
  (setq b-current-bookmark number))

(defun b-next-bookmark (&optional arg)
  "Jump to the next bookmark.
With ARG jump to the previous one."
  (interactive "P")
  (block b-next-bookmark	  	; Annoying that this is necessary...
    (when b-current-bookmark
      ;; Work out if we're going forwards or backwards through the bookmarks
      (let ((dir-fn (if arg #'- #'+)))
	;; Find the next bookmark in that direction
	(dotimes (i b-max-bookmarks)	; `dotimes' lexical-binding bug stops me using the result form now
	  (let* ((number (mod (funcall dir-fn b-current-bookmark i 1) b-max-bookmarks))
		 (bookmark (b-get-bookmark number)))
	    (when (b-valid-bookmark-p bookmark)
	      (b-jump-to-bookmark number)
	      (return-from b-next-bookmark))))))
    (user-error "No bookmarks have been set")))

(defun b-prev-bookmark (&optional arg)
  "Jump to the previous bookmark.
With ARG jump to the next one."
  (interactive "P")
  (b-next-bookmark (null arg)))

(defun b-list-bookmarks ()
  "Show list of all bookmarks."
  (interactive)
  (unless b-current-bookmark
    (user-error "No bookmarks have been set"))

  ;; List selection buffer is provided by `generic-menu'
  ;; *** Mike: Fix Me ***: Replace this with something more widely available (or my own code)
  (if (require 'generic-menu "generic-menu" t)
      (let ((select-bookmark (lambda (idx)
			       (let ((bookmark (b-get-bookmark idx)))
				 (cond ((b-valid-bookmark-p bookmark)
					(gm-quit)
					(b-jump-to-bookmark idx))
				       (t ; Bookmark not set
					(user-error "Bookmark %d is not set" idx))))))
	    (display-bookmark (lambda (idx)
				(let ((bookmark (b-get-bookmark idx)))
				  (cond ((b-valid-bookmark-p bookmark)
					 (let ((marker (b-bookmark-marker bookmark)))
					   (with-current-buffer (marker-buffer marker)
					     (save-excursion
					       (goto-char marker)
					       (format "%s %d\tL%d\tC%d\t%d%%\t%s"
						       (if (= b-current-bookmark idx) "*" " ")
						       idx
						       (b-current-line) (b-current-column)
						       (/ (* (point) 100) (buffer-size))
						       (buffer-name))))))
					(t ; Bookmark not set
					 (format "  %d <NOT SET>" idx)))))))
	(gm-popup :buffer-name "*Bookmarks*"
		  :header-line "Bookmarks: [SELECT] to Jump to bookmark, [q] to Quit."
		  :max-entries b-max-bookmarks
		  :truncate-lines t
		  :regexp-start-position (format "^[* ][ \t]+%d" b-current-bookmark)

		  :elements (loop for idx from 0 to (1- b-max-bookmarks)
				  collect idx)

		  :select-callback select-bookmark
		  :display-string-function display-bookmark))

    ;; `generic-menu' not available
    (declare-function gm-popup "ext:generic-menu" (&rest plain-properties))
    (declare-function gm-quit "ext:generic-menu" nil)
    (user-error "Please install generic-menu")))

(defun b-current-line ()
  "Return current line number of point (starting at 1)."
  (+ (count-lines 1 (point))
     (if (= (current-column) 0) 1 0)))

(defun b-current-column ()
  "Return current column of point (starting at 1)."
  (1+ (current-column)))

(provide 'b-bookmark)

;;; b-bookmark.el ends here
