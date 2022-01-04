;;; brf-marking.el --- Marking / Cut & Paste commands of brf-mode -*- lexical-binding: t -*-

;; Copyright (C) 1999-2022 Mike Woolley
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

;; See README.org or Info manual for further details.

;;; Code:

(require 'brf-compat)
(eval-when-compile (require 'cl-lib))

(defvar brf-line-mark-min nil
  "The minimum position of the mark in line marking mode.
The mark is positioned here if point is below this line.")
(defvar brf-line-mark-max nil
  "The maximum position of the mark in line marking mode.
The mark is positioned here if point is above or on this line.")
(defvar brf-line-mark-col nil
  "The original column where line marking was initiated.
This is restored after saving/killing the region.")
(defvar brf-line-mark-old-point nil
  "The location of point after a command is executed when line-marking.")
(make-variable-buffer-local 'brf-line-mark-old-point)
(defvar brf-line-mark-old-mark nil
  "The location of mark after a command is executed when line-marking.")
(make-variable-buffer-local 'brf-line-mark-old-mark)

;;
;; Line kill helper functions
;;
(defmacro brf-set-line-kill (place)
  "Make the string at PLACE a line-mode kill.
This is done by adding a text property and ensuring that the (last)
line is terminated with a newline."
  ;; Ensure the line is terminated with a newline
  `(let ((line (brf-terminate-line ,place)))
     ;; Indicate that this is a line-mode kill with a text property
     (put-text-property 0 1 'brf-line-kill t line)
     (setf ,place line)))

(defun brf-terminate-line (line)
  "Ensure LINE is terminated with a newline."
  (let ((len (length line)))
    (if (and (> len 0) (= (aref line (1- len)) ?\n))
	line
      (concat line "\n"))))

(defun brf-clear-line-kill (pos)
  "Remove the line-mode kill property from text at position POS in the buffer."
  (remove-text-properties pos (1+ pos) '(brf-line-kill t)))

(defun brf-line-kill-p (string)
  "Test if STRING is a line-mode kill."
  (when (stringp string)
    (get-text-property 0 'brf-line-kill string)))

;;
;; Line Marking Mode
;;
(defun brf-start-line-marking ()
  "Start line-marking mode."
  (when (brf-column-marking-p)
    (brf-stop-column-marking))
  (setq brf-line-mark-col (current-column))
  (when (and (fboundp 'make-local-hook)
	     (or brf-xemacs-flag (< emacs-major-version 21)))
    (make-local-hook 'post-command-hook)) ;; Not needed since Emacs 21
  (add-hook 'post-command-hook #'brf-mark-line-hook nil t)
  (setq brf-line-mark-old-point nil))

(defun brf-stop-line-marking (&optional delete-flag)
  "Stop line-marking mode, restoring point to the original column.
DELETE-FLAG indicates the line-marked region was deleted or killed."
  (brf-abort-line-marking)
  (unless delete-flag
    (when (> (point) (mark))
      (forward-line -1)))
  (move-to-column brf-line-mark-col))

(defun brf-abort-line-marking ()
  "Stop line-marking mode without adjusting point."
  (remove-hook 'post-command-hook #'brf-mark-line-hook t))

(defun brf-line-marking-p ()
  "Return non-nil if the buffer is in line marking mode."
  (memq 'brf-mark-line-hook post-command-hook))

(defun brf-mark-line-hook ()
  "Ensure point and mark are correctly positioned for line-marking after cursor motion commands."
  (when brf-line-mark-old-point
    ;; Check if line-marking has been implicitly ended by another command
    (cond ((or (brf-column-marking-p)
	       deactivate-mark	; The last command wants to deactivate the mark
	       (and (brf-region-active-p)
		    (/= (mark) brf-line-mark-old-mark)))
	   (brf-abort-line-marking))

	  ;; Line-marking
	  ;; Emulate Brief "line mode"
	  ((brf-region-active-p)
	   ;; No need to do anything if point hasn't moved
	   (when (/= (point) brf-line-mark-old-point)
	     ;; Stop `temporary-goal-column' interfering with the current column on [up]/[down] cursor movement
	     (setq temporary-goal-column 0)
	     ;; Ensure we're at the beginning of the line
	     (unless (bolp)
	       (beginning-of-line)
	       (when (>= (point) brf-line-mark-old-point)
		 (forward-line)))
	     ;; Ensure mark and point are straddling the original line
	     (let ((point (point))
		   (mark (mark)))
	       (cond ((< point mark)
		      (when (/= mark brf-line-mark-max)
			(set-mark brf-line-mark-max)))
		     ((> point mark)
		      (when (/= mark brf-line-mark-min)
			(set-mark brf-line-mark-min)))
		     ;; point = mark
		     ((= point brf-line-mark-max) ; point = mark-max
		      (forward-line 1)
		      (set-mark brf-line-mark-min))
		     (t				; point = mark-min
		      (forward-line -1)
		      (set-mark brf-line-mark-max)))
	       (brf-activate-region))))

	  ;; Not marking
	  (t
	   (brf-stop-line-marking))))

  ;; Save point & mark for next time
  (setq brf-line-mark-old-point (point))
  (setq brf-line-mark-old-mark (mark)))

(defun brf-mark-line (&optional arg)
  "Mark the current line from anywhere on the line.
Emulates the Brief mark-line function.
With ARG, do it that many times."
  (interactive "P")
  (if (not (brf-line-marking-p))
      ;; Line mark ARG lines
      (let ((lines (prefix-numeric-value arg)))
	(when (/= lines 0)
	  (brf-start-line-marking)
	  (beginning-of-line)
	  (setq brf-line-mark-min (point))
	  (forward-line)
	  (setq brf-line-mark-max (point))
	  (cond ((bolp)			; Normal case
		 (forward-line (1- lines))
		 (push-mark brf-line-mark-min t t))
		(t			; Case where last line is incomplete
		 (goto-char brf-line-mark-min)
		 (push-mark brf-line-mark-max t t)))
	  (message "Mark set (line mode)")))
    ;; Stop line-marking
    (brf-stop-line-marking)
    (brf-deactivate-region t)))

(defun brf-mark-default ()
  "Mark the default unit in the buffer.
Normally this is the current line, but in Lisp modes it is the containing sexp."
  (cond ((brf-lisp-mode-p)
	 (condition-case nil
	     (progn
	       (unless (= (following-char) ?\()
		 (backward-up-list))
	       (mark-sexp))
	   (error (brf-mark-line))))
	(t				; Non-lisp mode
	 (brf-mark-line))))

(defun brf-lisp-mode-p ()
  "Return non-nil if the current major mode is a Lisp mode.
This is determined heuristically by looking for `lisp' in the mode name."
  (string-match "lisp" (format-mode-line mode-name)))

(defun brf-emphasise-region (beg end &optional message-len)
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
(defun brf-column-marking-p ()
  "Return non-nil if the buffer is in column marking mode."
  (bound-and-true-p rectangle-mark-mode))

(defun brf-stop-column-marking ()
  "Stops column-marking mode."
  (when (fboundp 'rectangle-mark-mode)
    (rectangle-mark-mode -1)))

;;
;; Brief copy-region command
;;
(defun brf-copy-region ()
  "Copy the current active region to the kill ring.
If there is no active region then the current line is copied.
Emulates the Brief copy function."
  (interactive)
  (unless (brf-region-active-p)
    (brf-mark-default))
  (let ((beg (region-beginning))
	(end (region-end)))
    (copy-region-as-kill beg end t)	; It seems rectangle-mode needs the t arg, to process the region as a rectangle
    (brf-emphasise-region beg end)	; Emphasise the region like `kill-ring-save' does
    (when (brf-line-marking-p)
      (brf-set-line-kill (car kill-ring))
      (brf-stop-line-marking)))
  (brf-deactivate-region))

(defun brf-copy-to-register (register &optional delete-flag)
  "Copy the current active region to REGISTER.
With prefix arg DELETE-FLAG, delete as well.
If there is no active region then the current line is copied."
  (interactive (list (register-read-with-preview "Copy to register: ")
		     current-prefix-arg))
  (unless (brf-region-active-p)
    (brf-mark-default))
  (let ((beg (region-beginning))
	(end (region-end)))
    (if (brf-column-marking-p)
	(copy-rectangle-to-register register beg end delete-flag)
      (copy-to-register register beg end delete-flag))
    (unless delete-flag
      (brf-emphasise-region beg end))
    (when (brf-line-marking-p)
      (brf-set-line-kill (get-register register))
      (brf-stop-line-marking)))
  (brf-deactivate-region t))

;;
;; Brief kill-region command
;;
(defun brf-kill-region ()
  "Kill the current active region.
If there is no active region then the current line is killed.
Emulates the Brief cut function."
  (interactive "*")
  (unless (brf-region-active-p)
    (brf-mark-default))
  (kill-region (region-beginning) (region-end) t) ; Again rectangle-mode needs the t arg
  (when (brf-line-marking-p)
    (brf-set-line-kill (car kill-ring))
    (brf-stop-line-marking t)))

;;
;; Brief delete command
;;
(defun brf-delete (&optional arg)
  "Delete the current active region.
If there is no active region then ARG characters following point are deleted.
Emulates the Brief delete function."
  (interactive "*P")
  (cond ((brf-region-active-p)
	 ;; Delete the rectangle if one is active
	 (if (brf-column-marking-p)
	     (delete-rectangle (region-beginning) (region-end))
	   ;; Otherwise delete the current region
	   (delete-region (region-beginning) (region-end))
	   (when (brf-line-marking-p)
	     (brf-stop-line-marking t))))
	(t				; No active region
	 (delete-char (prefix-numeric-value arg)))))

;;
;; Brief Yank & Yank-pop commands
;;
(defvar brf-yank-col 0
  "The original column where `brf-yank' was initiated.
This is restored after the yank.")

(defvar brf-last-yank-was-line nil
  "Non-nil if the last yank was from a line-mode kill.")

(defmacro brf-insert-text (text insert)
  "Insert TEXT form into the current buffer using INSERT form.
Return non-nil if the inserted text was a line-kill."
  (declare (indent defun))
  (let ((line-kill (make-symbol "line-kill")))
    `(let ((,line-kill (brf-line-kill-p ,text)))
       ;; Pre-insert handling of line-kills
       (when ,line-kill
	 (setq brf-yank-col (current-column))
	 (beginning-of-line))
       ;; Insert the text
       ,insert
       ;; Post-insert handling of line-kills
       (when ,line-kill
	 (let ((point (point))
	       (mark (mark t)))
	   ;; Remove the line-kill property from the inserted text
	   (brf-clear-line-kill (min mark point))
	   ;; Indent the text
	   (when (brf-buffer-in-programming-mode-p)
	     (indent-region (min mark point) (max mark point) nil)))
	 ;; Restore the original column
	 (move-to-column brf-yank-col))
       ;; Return the value of line-kill
       ,line-kill)))

(defun brf-yank (&optional arg)
  "Similar to the normal `yank' ARG command.
However, correctly insert text that was killed in line-mode and
also indent it (if the buffer is in a programming mode)."
  (interactive "*P")
  (setq this-command 'yank)
  (setq brf-last-yank-was-line
	(brf-insert-text
	  (current-kill (cond ((listp arg) 0)
			      ((eq arg '-) -1)
			      (t (1- arg)))
			t)
	  (yank arg))))

(defun brf-yank-pop (arg)
  "Similar to the normal `yank-pop' ARG command.
However, correctly insert text that was killed in line-mode and
also indent it."
  (interactive "*p")
  (unless (eq last-command 'yank)
    (user-error "Previous command was not a yank"))
  (setq this-command 'yank)
  (let* ((text (current-kill arg t))
	 (line-kill (brf-line-kill-p text)))

    ;; Delete the last yank if `yank-pop' is not going to delete it correctly
    ;; Make sure point & mark are in the correct places for `brf-insert-text'
    (cond ((and brf-last-yank-was-line (not line-kill))
	   (delete-region (brf-bol-position) (mark t))
	   (set-mark (point)))
	  ((and (not brf-last-yank-was-line) line-kill)
	   (if (null yank-undo-function)
	       (delete-region (point) (mark t))
	     ;; Last yank was column-mode
	     (funcall yank-undo-function (min (point) (mark t)) (max (point) (mark t)))
	     (setq yank-undo-function nil))
	   (set-mark (brf-bol-position))))

    (setq brf-last-yank-was-line
	  (brf-insert-text
	    text
	    (yank-pop arg)))))

(defun brf-insert-register (register)
  "Similar to the normal `insert-register' REGISTER command.
However, correctly insert text that was killed in line-mode and
also indent it."
  (interactive "*cInsert Register:")
  (brf-insert-text
    (get-register register)
    (insert-register register (not current-prefix-arg))))

(defadvice menu-bar-select-yank (around brf-menu-bar-select-yank)
  "Override `menu-bar-select-yank' command to correctly handle line-mode text."
  (interactive "*")
  (brf-insert-text
    last-command-event
    ad-do-it))

;;
;; Utilities
;;
(defun brf-buffer-in-programming-mode-p ()
  "Return non-nil if the current buffer is in a programming major mode."
  (not (or (eq indent-line-function 'indent-to-left-margin)
	   (eq indent-line-function 'indent-relative))))

(provide 'brf-marking)

;; Local Variables:
;; tab-width: 8
;; indent-tabs-mode: t
;; End:

;;; brf-marking.el ends here
