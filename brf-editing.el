;;; brf-editing.el --- Editing commands of brf-mode -*- lexical-binding: t -*-

;; Copyright (C) 1999-2025 Mike Woolley
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
(require 'brf-marking)

;;
;; Brief insert-line command
;;
(defun brf-insert-line (&optional arg)
  "Open a new line underneath the current one and indent point.
Do not break current line.  Emulates the Brief insert-line function.
With ARG, do not indent."
  (interactive "*P")
  (end-of-line)
  (if arg
      (newline)
    (call-interactively (key-binding "\C-m"))))

;;
;; Brief delete-line command
;;
(defun brf-delete-line (&optional arg)
  "Delete the current line from anywhere on the line.
Emulates the Brief delete-line function.
With ARG, do it that many times."
  (interactive "*P")
  (let ((count (prefix-numeric-value arg))
	(column (current-column))
	start end)
    (beginning-of-line)
    (setq start (point))
    (forward-line count)
    (setq end (point))
    (delete-region start end)
    (move-to-column column)))

;;
;; Brief kill-line command
;;
;; Note that this command is not currently mapped to a key, as its
;; functionality is subsumed by brf-kill-region.
;;
(defun brf-kill-line (&optional arg)
  "Kill the current line from anywhere on the line.
With ARG, do it that many times."
  (interactive "*P")
  (let ((count (prefix-numeric-value arg))
	(column (current-column)))
    (beginning-of-line)
    (kill-line count)
    (brf-set-line-kill (car kill-ring))
    (move-to-column column)))

;;
;; Tab key handling
;;
(defun brf-tab (&optional arg)
  "Indent the region if the region is active.
Otherwise call the function which would ordinarily be bound to
the tab key with ARG."
  (interactive "P")
  (let ((tab-fn (or (local-key-binding "\t")
		      (global-key-binding "\t"))))
    (cond ((or (null tab-fn) (brf-indent-cmd-p tab-fn))
	   ;; If the region is active, indent it
	   (if (brf-region-active-p)
	       (indent-region (region-beginning) (region-end) arg)
	     ;; Otherwise, call the usual binding for the tab key
	     (if tab-fn
		 (if arg
		     (funcall tab-fn arg)
		   (funcall tab-fn))
	       ;; No binding found, so call sensible default
	       (indent-for-tab-command arg))))
	  (t ; The normal binding is not an indent command, so just invoke it
	   (if arg
	       (funcall tab-fn arg)
	     (funcall tab-fn))))))

(defun brf-indent-cmd-p (cmd)
  "Non-nil if CMD is an indent command, nil otherwise.
This is determined heuristically by seeing if the command name contains
the word \"indent\"."
  (and (symbolp cmd) (string-match "indent" (symbol-name cmd))))

(provide 'brf-editing)

;; Local Variables:
;; tab-width: 8
;; indent-tabs-mode: t
;; End:

;;; brf-editing.el ends here
