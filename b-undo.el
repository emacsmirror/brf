;;; b-undo.el --- Brief Cursor Motion Undo

;; Copyright (C) 2000, 2001, 2002 Mike Woolley
;; Author: Mike Woolley <mike@bulsara.com>
;; Version: $Id$

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
;;  See b.el

;;; Code:

(require 'b-compat)

(defcustom b-undo-enable nil
  "Enable cursor motion undo."
  :type 'boolean
  :set (lambda (symbol value)
	 (if value
	     (add-hook 'post-command-hook 'b-undo-post-command-hook)
	   (remove-hook 'post-command-hook 'b-undo-post-command-hook))
	 (set-default symbol value))
  :initialize 'custom-initialize-default
  :group 'b)

(defvar b-undo-point 0
  "The location of point after a command is executed.")
(make-variable-buffer-local 'b-undo-point)
(defvar b-undo-list-head nil
  "The head of the undo list after a command is executed.")
(make-variable-buffer-local 'b-undo-list-head)
;; (defvar b-undo-list-second nil
;;   "The second element of the undo list after a command is executed.")
;; (make-variable-buffer-local 'b-undo-list-second)

(defvar b-undo-debug-enabled nil)

(defun b-undo-post-command-hook ()
  "Post-command hook to remember what to undo."
  (when (listp buffer-undo-list)
    (let ((point (point))
	  (head (car buffer-undo-list)))
      ;; Put point on the undo list if necessary
      (unless (or (eq this-command 'undo) (eq this-command 'redo))
	(when (and (/= b-undo-point 0)
		   (/= point b-undo-point)
		   (equal head b-undo-list-head))
	  (when head
	    (undo-boundary))
	  (setq buffer-undo-list (cons b-undo-point buffer-undo-list))))

      ;; Save point and the undo-list head for next time
      (setq b-undo-point point)
      (setq head (car buffer-undo-list))
      (setq b-undo-list-head (if (and (consp head) (integerp (car head)))
				 (cons (car head) (cdr head))
			       head)))
    ;; Debug output
    (when b-undo-debug-enabled
      (b-undo-debug))))

(defun b-undo-debug ()
  "Show Undo state in a buffer for debugging."
  (unless (active-minibuffer-window)
    (let ((undo-list buffer-undo-list)
	  (pending pending-undo-list)
	  (point (point)))
      (save-current-buffer
	(set-buffer (get-buffer-create "*B Debug*"))
	(insert "(" (number-to-string point) ") List: " (prin1-to-string undo-list) ?\n)
	(when (or (eq this-command 'undo) (eq this-command 'redo))
	  (insert "Pending: " (prin1-to-string (car pending)) ?\n))
	(goto-char 1)))))

(defun b-undo-toggle-debug (&optional arg)
  "Turn Undo debugging on or off with ARG."
  (interactive "P")
  (setq b-undo-debug-enabled
	(if (null arg)
	    (not b-undo-debug-enabled)
	  (> (prefix-numeric-value arg) 0)))

  (when b-undo-debug-enabled
    (save-current-buffer
      (let ((buffer (get-buffer-create "*B Debug*")))
	(set-buffer buffer)
	(buffer-disable-undo buffer)
	(erase-buffer)
	(display-buffer buffer t)))))

(provide 'b-undo)

;;; b-undo.el ends here
