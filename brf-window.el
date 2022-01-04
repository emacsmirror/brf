;;; brf-window.el --- Brief-style Window Management -*- lexical-binding: t -*-

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

;;  Brief-style window management, using the Gnu Emacs builtin
;;  `window' and `windmove' libraries.

;;; Code:

;;
;; Change window
;;
(defun brf-change-window-up ()
  "Select the window above the current one."
  (interactive)
  (windmove-up))

(defun brf-change-window-down ()
  "Select the window below the current one."
  (interactive)
  (windmove-down))

(defun brf-change-window-left ()
  "Select the window to the left of the current one."
  (interactive)
  (windmove-left))

(defun brf-change-window-right ()
  "Select the window to the right of the current one."
  (interactive)
  (windmove-right))

;;
;; Resize window
;;
(defvar f2-prefix
  (let ((map (make-sparse-keymap)))
    (define-key map [(up)] 'brf-resize-window-up)
    (define-key map [(down)] 'brf-resize-window-down)
    (define-key map [(left)] 'brf-resize-window-left)
    (define-key map [(right)] 'brf-resize-window-right)
    (define-key map [t] 'brf-resize-window-end)		; Any other key ends resizing
    map)
  "Keymap for resizing windows.")

(defvar brf-resize-exit-fn nil
  "The function to call to deactivate the resizing map.")

(defvar brf-resize-edge nil
  "Window who's trailing edge is being resized.")

(defun brf-resize-window-start ()
  "Start interactive window resizing."
  (interactive)
  (message "Select an edge to move (use cursor keys)")
  (setq brf-resize-exit-fn
	(set-transient-map f2-prefix t 'brf-resize-window-exit)))

(defun brf-resize-window-end ()
  "End interactive window resizing."
  (interactive)
  (discard-input)
  (funcall brf-resize-exit-fn))

(defun brf-resize-window-exit ()
  "Called when window resizing has ended."
  (setq brf-resize-edge nil)
  (message "Window resizing done"))

(defun brf-resize-window (dir delta)
  "Handle cursor movement DELTA in direction DIR."
  (let ((horizontal   (or (eq dir 'left) (eq dir 'right)))	; Horizontal resize
	(resize-other (or (eq dir 'above) (eq dir 'left))))	; Edge of other window will be resized
    (message "Move edge to new position and press Enter")
    (if brf-resize-edge
	;; Move the edge that we're resizing in DIR by DELTA
	(adjust-window-trailing-edge brf-resize-edge
				     (if resize-other (- delta) delta)
				     horizontal)
      ;; Select the edge in DIR as the one to resize
      (let ((win (window-in-direction dir)))
	(if win
	    (setq brf-resize-edge (if resize-other win (selected-window)))
	  ;; No edge in that DIR
	  (funcall brf-resize-exit-fn)
	  (user-error "No re-sizable edge in that direction"))))))

(defun brf-resize-window-up (delta)
  "Handle cursor movement DELTA in upwards direction."
  (interactive "p")
  (brf-resize-window 'above delta))

(defun brf-resize-window-down (delta)
  "Handle cursor movement DELTA in downwards direction."
  (interactive "p")
  (brf-resize-window 'below delta))

(defun brf-resize-window-left (delta)
  "Handle cursor movement DELTA in leftwards direction."
  (interactive "p")
  (brf-resize-window 'left delta))

(defun brf-resize-window-right (delta)
  "Handle cursor movement DELTA in rightwards direction."
  (interactive "p")
  (brf-resize-window 'right delta))

;;
;; Zoom Window Toggle
;;
(defvar brf-zoom-window-config nil
  "Window configuration saved by Zoom Window Toggle.")

(defun brf-zoom-window ()
  "Toggle making the current window full screen."
  (interactive)
  (if (= (count-windows) 1)
      (when brf-zoom-window-config
	(set-window-configuration brf-zoom-window-config)
	(setq brf-zoom-window-config nil))
    (setq brf-zoom-window-config (current-window-configuration))
    (delete-other-windows)))

;;
;; Create window
;;
(defun brf-create-window-up ()
  "Create a new window above the current window."
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun brf-create-window-down ()
  "Create a new window below the current window."
  (interactive)
  (split-window-vertically))

(defun brf-create-window-left ()
  "Create a new window on the left of the current window."
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun brf-create-window-right ()
  "Create a new window on the right of the current window."
  (interactive)
  (split-window-horizontally))

;;
;; Delete window
;;
(defun brf-delete-window (dir)
  "Delete the window in direction DIR from the current window."
  (let ((window (window-in-direction dir)))
    (if window
	(delete-window window)
      (user-error "No window %s from selected window" dir))))

(defun brf-delete-window-up ()
  "Delete the window above the current window."
  (interactive)
  (brf-delete-window 'above))

(defun brf-delete-window-down ()
  "Delete the window below the current window."
  (interactive)
  (brf-delete-window 'below))

(defun brf-delete-window-left ()
  "Delete the window to the left of the current window."
  (interactive)
  (brf-delete-window 'left))

(defun brf-delete-window-right ()
  "Delete the window to the right of the current window."
  (interactive)
  (brf-delete-window 'right))

(defun brf-delete-current-window ()
  "Delete the current window."
  (interactive)
  (delete-window)
  (other-window -1))

(provide 'brf-window)

;; Local Variables:
;; tab-width: 8
;; indent-tabs-mode: t
;; End:

;;; brf-window.el ends here
