;;; b-marking.el --- Marking / Cut & Paste commands of b-mode -*- lexical-binding: t -*-

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

(defvar b-line-mark-min nil
  "The minimum position of the mark in line marking mode.
The mark is positioned here if point is below this line.")
(defvar b-line-mark-max nil
  "The maximum position of the mark in line marking mode.
The mark is positioned here if point is above or on this line.")
(defvar b-line-mark-col nil
  "The original column where line marking was initiated.
This is restored after saving/killing the region.")

;;
;; Line kill helper functions
;;
(defmacro b-set-line-kill (place)
  "Make the string at PLACE a line-mode kill.
This is done by adding a text property and ensuring that the (last)
line is terminated with a newline."
  ;; Ensure the line is terminated with a newline
  `(let ((line (b-terminate-line ,place)))
     ;; Indicate that this is a line-mode kill with a text property
     (put-text-property 0 1 'b-line-kill t line)
     (setf ,place line)))

(defun b-terminate-line (line)
  "Ensure LINE is terminated with a newline."
  (let ((len (length line)))
    (if (and (> len 0) (= (aref line (1- len)) ?\n))
	line
      (concat line "\n"))))

(defun b-clear-line-kill (pos)
  "Remove the line-mode kill property from text at position POS in the buffer."
  (remove-text-properties pos (1+ pos) '(b-line-kill t)))

(defun b-line-kill-p (string)
  "Test if STRING is a line-mode kill."
  (when (stringp string)
      (get-text-property 0 'b-line-kill string)))

;;
;; Line Marking Mode
;;
(defun b-start-line-marking ()
  "Start line-marking mode."
  (when (b-column-marking-p)
    (b-stop-column-marking))
  (setq b-line-mark-col (current-column))
  (when (and (fboundp 'make-local-hook)
	     (or b-xemacs-flag (< emacs-major-version 21)))
      (make-local-hook 'post-command-hook)) ;; Not needed since Emacs 21
  (add-hook 'post-command-hook #'b-mark-line-hook nil t))

(defun b-stop-line-marking ()
  "Stops line-marking mode."
  (remove-hook 'post-command-hook #'b-mark-line-hook t)
  (move-to-column b-line-mark-col))

(defun b-line-marking-p ()
  "Return non-nil if the buffer is in line marking mode."
  (memq 'b-mark-line-hook post-command-hook))

(defun b-mark-line-hook ()
  "Ensure point and mark are correctly positioned for line-marking after cursor motion commands."
  (cond ((and (b-region-active-p) (not (b-column-marking-p)))
	 ;; Marking - emulate Brief "line mode"
	 (let ((point (point))
	       (mark (mark)))
	   ;; Ensure we're at the beginning of the line
	   (unless (bolp)
	     (beginning-of-line)
	     (when (> point mark)
	       (forward-line)))

	   ;; Ensure mark and point are straddling the original line
	   (cond ((< point mark)
		  (when (/= mark b-line-mark-max)
		    (set-mark b-line-mark-max)))
		 ((> point mark)
		  (when (/= mark b-line-mark-min)
		    (set-mark b-line-mark-min)))
		 ;; point = mark
		 ((= point b-line-mark-max) ; point = mark-max
		  (forward-line 1)
		  (set-mark b-line-mark-min))
		 (t			; point = mark-min
		  (forward-line -1)
		  (set-mark b-line-mark-max)))

	   (b-activate-region)))
	(t				; Not marking
	 (b-stop-line-marking))))

(defun b-mark-line (&optional arg)
  "Mark the current line from anywhere on the line.
Emulates the Brief mark-line function.
With ARG, do it that many times."
  (interactive "P")
  (let ((lines (prefix-numeric-value arg)))
    (when (/= lines 0)
      (b-start-line-marking)
      (beginning-of-line)
      (setq b-line-mark-min (point))
      (forward-line)
      (setq b-line-mark-max (point))
      (cond ((bolp)			; Normal case
	     (forward-line (1- lines))
	     (push-mark b-line-mark-min nil t))
	    (t			  ; Case where last line is incomplete
	     (goto-char b-line-mark-min)
	     (push-mark b-line-mark-max nil t))))))

(defun b-mark-default ()
  "Mark the default unit in the buffer.
Normally this is the current line, but in Lisp modes it is the containing sexp."
  (cond ((b-lisp-mode-p)
	 (condition-case nil
	     (progn
	       (unless (= (following-char) ?\()
		 (backward-up-list 1 t t))
	       (mark-sexp))
	   (error (b-mark-line))))
	(t				; Non-lisp mode
	 (b-mark-line))))

(defun b-lisp-mode-p ()
  "Return non-nil if the current major mode is a Lisp mode.
This is determined heuristically by looking for `lisp' in the mode name."
  (string-match "lisp" (format-mode-line mode-name)))

(defun b-emphasise-region (beg end &optional message-len)
  "Emphasise the region BEG END, like `kill-ring-save' does.

If the mark lies outside the selected window, display an
informative message containing a sample of the copied text.  The
optional argument MESSAGE-LEN, if non-nil, specifies the length
of this sample text; it defaults to 40."
  ;; This is loosely based on code in `kill-ring-save' from simple.el in GNU Emacs
  (let ((other-end (if (= (point) beg) end beg))
	(opoint (point))
	(inhibit-quit t))		; Inhibit quitting
    (cond ((pos-visible-in-window-p other-end (selected-window))
	   ;; Swap point and mark.
	   (set-marker (mark-marker) (point) (current-buffer))
	   (goto-char other-end)
	   (sit-for blink-matching-delay)
	   ;; Swap back.
	   (set-marker (mark-marker) other-end (current-buffer))
	   (goto-char opoint))
	  (t ; Other end of region not visible
	   (let ((len (min (abs (- other-end opoint))
			   (or message-len 40))))
	     (if (< opoint other-end)
		 (message "Saved text until \"%s\""
			  (buffer-substring-no-properties (- other-end len) other-end))
	       (message "Saved text from \"%s\""
			(buffer-substring-no-properties other-end (+ other-end len)))))))))

;;
;; Column Marking Mode
;;
;; Now `rectangle-mark-mode' has been added to Gnu Emacs, I'm just using that for column marking :-)
;;
(defun b-column-marking-p ()
  "Return non-nil if the buffer is in column marking mode."
  (and (boundp 'rectangle-mark-mode) rectangle-mark-mode))

(defun b-stop-column-marking ()
  "Stops column-marking mode."
  (when (fboundp 'rectangle-mark-mode)
      (rectangle-mark-mode -1)))

;;
;; Brief copy-region command
;;
(defun b-copy-region ()
  "Copy the current active region to the kill ring.
If there is no active region then the current line is copied.
Emulates the Brief copy function."
  (interactive)
  (unless (b-region-active-p)
    (b-mark-default))
  (let ((beg (region-beginning))
	(end (region-end)))
    (copy-region-as-kill beg end t)	; It seems rectangle-mode needs the t arg, to process the regiona as a rectangle
    (b-emphasise-region beg end)	; Emphasise the region like `kill-ring-save' does
    (when (b-line-marking-p)
      (b-set-line-kill (car kill-ring))
      (b-stop-line-marking)
      (when (> (point) (mark))
	(forward-line -1)
	(move-to-column b-line-mark-col))))
  (b-deactivate-region))

(defun b-copy-to-register (register &optional delete-flag)
  "Copy the current active region to REGISTER.
With prefix arg DELETE-FLAG, delete as well.
If there is no active region then the current line is copied."
  (interactive (list (register-read-with-preview "Copy to register: ")
		     current-prefix-arg))
  (unless (b-region-active-p)
    (b-mark-default))
  (let ((beg (region-beginning))
	(end (region-end)))
    (if (b-column-marking-p)
	(copy-rectangle-to-register register beg end delete-flag)
      (copy-to-register register beg end delete-flag))
    (b-emphasise-region beg end)
    (when (b-line-marking-p)
      (b-set-line-kill (get-register register))
      (b-stop-line-marking)
      (when (> (point) (mark))
	(forward-line -1)
	(move-to-column b-line-mark-col))))
  (b-deactivate-region t))

;;
;; Brief kill-region command
;;
(defun b-kill-region ()
  "Kill the current active region.
If there is no active region then the current line is killed.
Emulates the Brief cut function."
  (interactive "*")
  (unless (b-region-active-p)
    (b-mark-default))
  (kill-region (region-beginning) (region-end) t) ; Again rectangle-mode needs the t arg
  (when (b-line-marking-p)
    (b-set-line-kill (car kill-ring))
    (b-stop-line-marking)))

;;
;; Brief delete command
;;
(defun b-delete (&optional arg)
  "Delete the current active region.
If there is no active region then ARG characters following point are deleted.
Emulates the Brief delete function."
  (interactive "*P")
  (cond ((b-region-active-p)
	 ;; Delete the rectangle if one is active
	 (if (b-column-marking-p)
	     (delete-rectangle (region-beginning) (region-end))
	   ;; Otherwise delete the current region
	   (delete-region (region-beginning) (region-end))
	   (when (b-line-marking-p)
	     (b-stop-line-marking))))
	(t				; No active region
	 (delete-char (prefix-numeric-value arg)))))

;;
;; Brief Yank & Yank-pop commands
;;
(defvar b-yank-col 0
  "The original column where `b-yank' was initiated.
This is restored after the yank.")

(defvar b-last-yank-was-line nil
  "Non-nil if the last yank was from a line-mode kill.")

(defun b-yank (&optional arg)
  "Similar to the normal `yank' ARG command.
However, correctly insert text that was killed in line-mode and
also indent it (if the buffer is in a programming mode)."
  (interactive "*P")
  (setq this-command 'yank)
  (setq b-yank-col (current-column))
  (cond ((b-line-kill-p (current-kill (cond ((listp arg) 0)
					    ((eq arg '-) -1)
					    (t (1- arg))) t))
	 (beginning-of-line)
	 (yank arg)
	 (let ((point (point))
	       (mark (mark t)))
	   (b-clear-line-kill (min mark point))
	   (when (b-buffer-in-programming-mode-p)
	     (indent-region (min mark point) (max mark point) nil)))
	 (setq b-last-yank-was-line t)
	 (move-to-column b-yank-col))

	(t				; Not line kill
	 (yank arg)
	 (setq b-last-yank-was-line nil))))

(defun b-yank-pop (arg)
  "Similar to the normal `yank-pop' ARG command.
However, correctly insert text that was killed in line-mode and
also indent it."
  (interactive "*p")
  (unless (eq last-command 'yank)
    (user-error "Previous command was not a yank"))
  (setq this-command 'yank)
  (cond ((b-line-kill-p (current-kill arg t))
	 (cond (b-last-yank-was-line
		(beginning-of-line))
	       (t
		(delete-region (point) (mark t))
		(beginning-of-line)
		(set-mark (point))
		(setq b-last-yank-was-line t)))
	 (yank-pop arg)
	 (let ((point (point))
	       (mark (mark t)))
	   (b-clear-line-kill (min mark point))
	   (when (b-buffer-in-programming-mode-p)
	     (indent-region (min mark point) (max mark point) nil)))
	 (move-to-column b-yank-col))

	(t				; Not line kill
	 (when b-last-yank-was-line
	   (beginning-of-line)
	   (delete-region (point) (mark t))
	   (move-to-column b-yank-col)
	   (set-mark (point))
	   (setq b-last-yank-was-line nil))
	 (yank-pop arg))))

(defun b-insert-register (register)
  "Similar to the normal `insert-register' REGISTER command.
However, correctly insert text that was killed in line-mode and
also indent it."
  (interactive "*cInsert Register:")
  (let ((line-kill (b-line-kill-p (get-register register))))
    (when line-kill
      (setq b-yank-col (current-column))
      (beginning-of-line))

    (insert-register register (not current-prefix-arg))

    (when line-kill
      (let ((point (point))
	    (mark (mark t)))
	(b-clear-line-kill (min mark point))
	(when (b-buffer-in-programming-mode-p)
	  (indent-region (min mark point) (max mark point) nil)))
      (move-to-column b-yank-col))))

;;
;; Utilities
;;
(defun b-buffer-in-programming-mode-p ()
  "Return non-nil if the current buffer is in a programming major mode."
  (not (or (eq indent-line-function 'indent-to-left-margin)
	   (eq indent-line-function 'indent-relative))))

(provide 'b-marking)

;;; b-marking.el ends here
