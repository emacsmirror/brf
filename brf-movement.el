;;; brf-movement.el --- Paging & Scrolling features of brf-mode -*- lexical-binding: t -*-

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

;; Brief paging and scrolling commands
;; These commands aim to provide fully reversible paging, so that point
;; returns to the same position after paging up and down.
;;

;;; Code:

(require 'brf-compat)
(eval-when-compile (require 'cl-lib))

(defvar brf-temporary-goal-column 0
  "Original column of the start of a sequence Brf scrolling commands.")

(defun brf-emacs-scrolling-p ()
  "Return non-nil if we're using Emacs scrolling rather than our scrolling."
  (bound-and-true-p scroll-preserve-screen-position))

(defun brf-window-height ()
  "Return the window height in lines, respecting the current line spacing."
  (let ((window-height (window-body-height (selected-window) t))
	(line-height (frame-char-height)))
    (cond ((floatp line-spacing)
	   (setq line-height (* line-height (1+ line-spacing))))
	  ((integerp line-spacing)
	   (cl-incf line-height line-spacing))
	  (t
	   (cl-assert (null line-spacing) nil "Unknown line-spacing type!")))
    (round window-height line-height)))

(defun brf-page-down (&optional arg)
  "Page the current window down, respecting `next-screen-context-lines'.
Paging up afterwards should return point to the same position.
The optional ARG specifies the number of pages to scroll."
  (interactive "P")
  (brf-keep-region)
  (let ((pages (prefix-numeric-value arg)))
    (if (< pages 0)
	(brf-page-up (- pages))
      (let ((lines (max (- (brf-window-height)
			   next-screen-context-lines)
			1)))
	(while (and (> pages 0) (not (pos-visible-in-window-p (point-max))))
	  (brf-scroll-screen lines)
	  (cl-decf pages))))))
(put 'brf-page-down 'brf-scroll-command t)

(defun brf-page-up (&optional arg)
  "Page the current window up, respecting `next-screen-context-lines'.
Paging down afterwards should return point to the same position.
The optional ARG specifies the number of pages to scroll."
  (interactive "P")
  (brf-keep-region)
  (let ((pages (prefix-numeric-value arg)))
    (if (< pages 0)
	(brf-page-down (- pages))
      (let ((lines (min (- next-screen-context-lines
			   (brf-window-height))
			-1)))
	(while (and (> pages 0) (not (pos-visible-in-window-p (point-min))))
	  (brf-scroll-screen lines)
	  (cl-decf pages))))))
(put 'brf-page-up 'brf-scroll-command t)

(defun brf-scroll-screen (lines)
  "Scroll current window by LINES, saving the cursor's relative window position.
This is a helper function used by `brf-page-up' and `brf-page-down'.
It should still work in the presence of hidden lines."
  (unless (brf-scroll-command-p last-command)
    (setq brf-temporary-goal-column (current-column)))
  (if (brf-emacs-scrolling-p)
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
  (move-to-column brf-temporary-goal-column))

(defun brf-row-up (&optional arg)
  "Scroll the current window down by one line.
ARG specifies the number of lines to scroll, defaulting to 1."
  (interactive "P")
  (brf-keep-region)
  (brf-scroll-line (prefix-numeric-value arg)))
(put 'brf-row-up 'brf-scroll-command t)

(defun brf-row-down (&optional arg)
  "Scroll the current window up by one line.
ARG specifies the number of lines to scroll, defaulting to 1."
  (interactive "P")
  (brf-keep-region)
  (brf-scroll-line (- (prefix-numeric-value arg))))
(put 'brf-row-down 'brf-scroll-command t)

(defun brf-scroll-line (lines)
  "Scroll the current window down by LINES.
This is a helper function used by brf-row-up/down."
  (unless (brf-scroll-command-p last-command)
    (setq brf-temporary-goal-column (current-column)))
  (scroll-down lines)
  (move-to-column brf-temporary-goal-column))

(defun brf-scroll-command-p (cmd)
  "Non-nil if CMD is a Brf scrolling command."
  (and (symbolp cmd) (get cmd 'brf-scroll-command)))

;;
;; Brief Home/End key functions
;;
(defvar brf-last-last-command nil
  "The previous value of `last-command'.
Used in the implementation of brf-home/end.")

(defun brf-home ()
  "\"Home\" the point, the way Brief does it.
The first use moves point to beginning of the line.  Second
consecutive use moves point to beginning of the screen.  Third
consecutive use moves point to the beginning of the buffer."
  (interactive)
  (if (eq last-command 'brf-home)
      (if (eq brf-last-last-command 'brf-home)
	  (goto-char (point-min))
	(move-to-window-line 0))
    (beginning-of-line))
  (setq brf-last-last-command last-command))

(defun brf-end ()
  "\"End\" the point, the way Brief does it.
The first use moves point to end of the line.  Second
consecutive use moves point to the end of the screen.  Third
consecutive use moves point to the end of the buffer."
  (interactive)
  (if (eq last-command 'brf-end)
      (if (eq brf-last-last-command 'brf-end)
	  (goto-char (point-max))
	(move-to-window-line -1))
    (end-of-line))
  (setq brf-last-last-command last-command))

(provide 'brf-movement)

;; Local Variables:
;; tab-width: 8
;; indent-tabs-mode: t
;; End:

;;; brf-movement.el ends here
