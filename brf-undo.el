;;; brf-undo.el --- Brief Cursor Motion Undo -*- lexical-binding: t -*-

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

;;  Implements Cursor Motion Undo just like in Brief.
;;  This works with both default Emacs Undo and Redo(+).el.
;;  Finally finished this off 20 years after I started it...

;;; Code:

(require 'brf-compat)

(defcustom brf-undo-enable nil
  "Enable cursor motion undo."
  :type 'boolean
  :set (lambda (symbol value)
	 (if value
	     (add-hook 'post-command-hook #'brf-undo-post-command-hook)
	   (remove-hook 'post-command-hook #'brf-undo-post-command-hook))
	 (set-default symbol value))
  :initialize 'custom-initialize-default
  :group 'brf)

(defvar brf-undo-point 0
  "The location of point after a command is executed.")
(make-variable-buffer-local 'brf-undo-point)
(defvar brf-undo-list-head nil
  "The first non-boundary item on the undo list after a command is executed.")
(make-variable-buffer-local 'brf-undo-list-head)

(defvar brf-undo-debug-enabled nil
  "Undo debug is enabled.")

(defconst brf-undo-debug-buffer-name "*Brf Debug*"
  "Name of undo debug buffer.")

(defun brf-undo-post-command-hook ()
  "Post-command hook to implement cursor-motion undo."
  ;; Put point on the undo list if necessary
  (when (listp buffer-undo-list)
    (let ((point (point))
	  (head (car buffer-undo-list)))
      ;; When head is not an undo boundary, Emacs is still amalgamating the undo group
      ;; So it's definitely not cursor motion
      (unless head
	(setq head (cadr buffer-undo-list))		; Real head is the second item
	(unless (eq this-command 'redo)			; ie (redo) from Redo(+).el
	  ;; Check if there was cursor motion with no other changes
	  (when (and (/= brf-undo-point 0)
		     (/= point brf-undo-point) 		; Point has moved
		     (or (eq head brf-undo-list-head) 	; and a change has not been made
			 (and (integerp head)		; or previous change was cursor motion
			      (null (caddr buffer-undo-list)))))
	    (setq buffer-undo-list (cons brf-undo-point buffer-undo-list))
	    ;; If we're undoing then the cursor motion was a redo, so mark it as such
	    (when (eq this-command 'undo)
	      (puthash buffer-undo-list
		       (if (or undo-in-region (eq buffer-undo-list pending-undo-list))
			   t
			 pending-undo-list)
		       undo-equiv-table))
	    ;; Add the terminal undo boundary
	    (undo-boundary))))

      ;; Save point and the undo-list head for next time
      (setq brf-undo-point point)
      (setq brf-undo-list-head head))

    ;; Debug output
    (when brf-undo-debug-enabled
      (brf-undo-debug))))

(defun brf-undo-debug ()
  "Show Undo state in a buffer for debugging."
  (unless (or (active-minibuffer-window)
	      (not (listp buffer-undo-list)))
    (let ((undo-list buffer-undo-list)
	  (pending pending-undo-list)
	  (point (point))
	  (buffer-name (buffer-name)))
      (let* ((buffer (get-buffer brf-undo-debug-buffer-name))
	     (window (get-buffer-window buffer t)))
	(when buffer
	  (save-current-buffer
	    (set-buffer buffer)
	    (goto-char (point-max))
	    (let ((log-point (point))	; Current point in the debug buffer
		  (print-length 15)	; Only show this many undo elements
		  (print-level 2))	; and only 2 deep
	      ;; Show point and the current undo-list
	      (insert buffer-name " (" (number-to-string point) "): " (prin1-to-string undo-list) ?\n)
	      ;; Also show what's on the pending list when we're undoing/redoing
	      (when (or (eq this-command 'undo)
			(eq this-command 'redo))
		(insert (prin1-to-string this-command) " pending: " (prin1-to-string pending) ?\n))
	      ;; Make sure the latest debug line is visible
	      (when (and (window-live-p window)
			 (not (pos-visible-in-window-p log-point window)))
		(set-window-start window log-point)))))))))

(defun brf-undo-toggle-debug (&optional arg)
  "Turn Undo debugging on or off with ARG."
  (interactive "P")
  (setq brf-undo-debug-enabled
	(if (null arg)
	    (not brf-undo-debug-enabled)
	  (> (prefix-numeric-value arg) 0)))

  (when brf-undo-debug-enabled
    (save-current-buffer
      (let ((buffer (get-buffer-create brf-undo-debug-buffer-name)))
	(set-buffer buffer)
	(buffer-disable-undo buffer)
	(erase-buffer)
	(display-buffer-at-bottom buffer nil)))))

(provide 'brf-undo)

;; Local Variables:
;; tab-width: 8
;; indent-tabs-mode: t
;; End:

;;; brf-undo.el ends here
