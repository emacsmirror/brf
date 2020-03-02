;;; b-movement.el --- Paging & Scrolling features of b-mode

;; Copyright (C) 2000-2020 Mike Woolley
;; Author: Mike Woolley <mike@bulsara.com>

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
;; Brief paging and scrolling commands
;; These commands aim to provide fully reversible paging, so that point
;; returns to the same position after paging up and down.
;;

;;; Code:

(require 'b-compat)
(eval-when-compile
  (require 'cl))

(defvar b-temporary-goal-column 0
  "Original column of the start of a sequence B scrolling commands.")

(defun b-emacs-scrolling-p ()
  "Return non-nil if we're using Emacs scrolling rather than our scrolling."
  (bound-and-true-p scroll-preserve-screen-position))

(defun b-window-height ()
  "Return the window height in lines, respecting the current line spacing."
  (let ((window-height (window-body-height (selected-window) t))
	(line-height (frame-char-height)))
    (cond ((floatp line-spacing)
	   (setq line-height (* line-height (1+ line-spacing))))
	  ((integerp line-spacing)
	   (incf line-height line-spacing))
	  (t
	   (assert (null line-spacing) nil "Unknown line-spacing type!")))
    (round window-height line-height)))

(defun b-page-down (&optional arg)
  "Page the current window down, respecting `next-screen-context-lines'.
Paging up afterwards should return point to the same position.
The optional ARG specifies the number of pages to scroll."
  (interactive "P")
  (b-keep-region)
  (let ((pages (prefix-numeric-value arg)))
    (if (< pages 0)
	(b-page-up (- pages))
      (let ((lines (max (- (b-window-height)
			   next-screen-context-lines)
			1)))
	(while (and (> pages 0) (not (pos-visible-in-window-p (point-max))))
	  (b-scroll-screen lines)
	  (decf pages))))))
(put 'b-page-down 'b-scroll-command t)

(defun b-page-up (&optional arg)
  "Page the current window up, respecting `next-screen-context-lines'.
Paging down afterwards should return point to the same position.
The optional ARG specifies the number of pages to scroll."
  (interactive "P")
  (b-keep-region)
  (let ((pages (prefix-numeric-value arg)))
    (if (< pages 0)
	(b-page-down (- pages))
      (let ((lines (min (- next-screen-context-lines
			   (b-window-height))
			-1)))
	(while (and (> pages 0) (not (pos-visible-in-window-p (point-min))))
	  (b-scroll-screen lines)
	  (decf pages))))))
(put 'b-page-up 'b-scroll-command t)

(defun b-scroll-screen (lines)
  "Scroll current window by LINES, saving the cursor's relative window position.
This is a helper function used by `b-page-up' and `b-page-down'.
It should still work in the presence of hidden lines."
  (unless (b-scroll-command-p last-command)
    (setq b-temporary-goal-column (current-column)))
  (if (b-emacs-scrolling-p)
      ;; Scroll using the core C Emacs functions
      (if (< lines 0) (scroll-down) (scroll-up))
    ;; Use our method of scrolling, which is less accurate when there
    ;; are mixed height fonts etc
    (let ((point (point)))
      (goto-char (window-start))
      (forward-line lines)
      (set-window-start (selected-window) (point))
      (goto-char point))
    (forward-line lines))
  (move-to-column b-temporary-goal-column))

(defun b-row-up (&optional arg)
  "Scroll the current window down by one line.
ARG specifies the number of lines to scroll, defaulting to 1."
  (interactive "P")
  (b-keep-region)
  (b-scroll-line (prefix-numeric-value arg)))
(put 'b-row-up 'b-scroll-command t)

(defun b-row-down (&optional arg)
  "Scroll the current window up by one line.
ARG specifies the number of lines to scroll, defaulting to 1."
  (interactive "P")
  (b-keep-region)
  (b-scroll-line (- (prefix-numeric-value arg))))
(put 'b-row-down 'b-scroll-command t)

(defun b-scroll-line (lines)
  "Scroll the current window down by LINES.
This is a helper function used by b-row-up/down."
  (unless (b-scroll-command-p last-command)
    (setq b-temporary-goal-column (current-column)))
  (scroll-down lines)
  (move-to-column b-temporary-goal-column))

(defun b-scroll-command-p (cmd)
  "Non-nil if CMD is a B scrolling command."
  (and (symbolp cmd) (get cmd 'b-scroll-command)))

;;
;; Brief Home/End key functions
;;
(defvar b-last-last-command nil
  "The previous value of `last-command'.
Used in the implementation of b-home/end.")

(defun b-home ()
  "\"Home\" the point, the way Brief does it.
The first use moves point to beginning of the line.  Second
consecutive use moves point to beginning of the screen.  Third
consecutive use moves point to the beginning of the buffer."
  (interactive)
  (if (eq last-command 'b-home)
      (if (eq b-last-last-command 'b-home)
	  (goto-char (point-min))
	(move-to-window-line 0))
    (beginning-of-line))
  (setq b-last-last-command last-command))

(defun b-end ()
  "\"End\" the point, the way Brief does it.
The first use moves point to end of the line.  Second
consecutive use moves point to the end of the screen.  Third
consecutive use moves point to the end of the buffer."
  (interactive)
  (if (eq last-command 'b-end)
      (if (eq b-last-last-command 'b-end)
	  (goto-char (point-max))
	(move-to-window-line -1))
    (end-of-line))
  (setq b-last-last-command last-command))

(provide 'b-movement)

;;; b-movement.el ends here
