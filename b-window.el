;;; b-window.el --- Brief-style Window Management -*- lexical-binding: t -*-

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
;;  Brief-style window management, implemented using the Gnu Emacs
;;  builtin `windmove' library.

;;; Code:

;;
;; Change window
;;
(defun b-change-window-up ()
  "Select the window above the current one."
  (interactive)
  (windmove-up))

(defun b-change-window-down ()
  "Select the window below the current one."
  (interactive)
  (windmove-down))

(defun b-change-window-left ()
  "Select the window to the left of the current one."
  (interactive)
  (windmove-left))

(defun b-change-window-right ()
  "Select the window to the right of the current one."
  (interactive)
  (windmove-right))

;;
;; Resize window
;;
(defun b-resize-window-up (delta)
  "Resize current window upwards by DELTA columns."
  (interactive "p")
  (if (window-in-direction 'above)
      (enlarge-window delta)
    (shrink-window delta)))

(defun b-resize-window-down (delta)
  "Resize current window downwards by DELTA columns."
  (interactive "p")
  (if (window-in-direction 'below)
      (enlarge-window delta)
    (shrink-window delta)))

(defun b-resize-window-left (delta)
  "Resize current window leftwards by DELTA columns."
  (interactive "p")
  (if (window-in-direction 'left)
      (enlarge-window-horizontally delta)
    (shrink-window-horizontally delta)))

(defun b-resize-window-right (delta)
  "Resize current window rightwards by DELTA columns."
  (interactive "p")
  (if (window-in-direction 'right)
      (enlarge-window-horizontally delta)
    (shrink-window-horizontally delta)))

;;
;; Create window
;;
(defun b-create-window-up ()
  "Create a new window above the current window."
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun b-create-window-down ()
  "Create a new window below the current window."
  (interactive)
  (split-window-vertically))

(defun b-create-window-left ()
  "Create a new window on the left of the current window."
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun b-create-window-right ()
  "Create a new window on the right of the current window."
  (interactive)
  (split-window-horizontally))

;;
;; Delete window
;;
(defun b-delete-window (dir)
  "Delete the window in direction DIR from the current window."
  (let ((window (window-in-direction dir)))
    (if window
	(delete-window window)
      (funcall (if (fboundp 'user-error) 'user-error 'error)
	       "No window %s from selected window" dir))))

(defun b-delete-window-up ()
  "Delete the window above the current window."
  (interactive)
  (b-delete-window 'above))

(defun b-delete-window-down ()
  "Delete the window below the current window."
  (interactive)
  (b-delete-window 'below))

(defun b-delete-window-left ()
  "Delete the window to the left of the current window."
  (interactive)
  (b-delete-window 'left))

(defun b-delete-window-right ()
  "Delete the window to the right of the current window."
  (interactive)
  (b-delete-window 'right))

(defun b-delete-current-window ()
  "Delete the current window."
  (interactive)
  (delete-window)
  (other-window -1))

(provide 'b-window)

;;; b-window.el ends here
