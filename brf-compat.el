;;; brf-compat.el --- XEmacs compatibility code for brf-mode -*- lexical-binding: t -*-

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

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (defconst brf-xemacs-flag (featurep 'xemacs)
    "Non-nil means this version of Emacs is XEmacs."))

;; Load XEmacs overlay compatibility
(when brf-xemacs-flag
  (require 'overlay))

;; Silence the byte compiler
(eval-when-compile
  (cond (brf-xemacs-flag
	 (defvar mark-active nil)
	 (defvar transient-mark-mode nil))
	(t ; GNU Emacs
	 (defvar zmacs-region-active-p nil)
	 (defvar zmacs-regions nil)
	 (defvar zmacs-region-stays nil)
	 (defun zmacs-update-region ())
	 (defun zmacs-activate-region ())
	 (defun zmacs-deactivate-region ()))))

;; Function to get the position of the beginning of the line
(cond ((fboundp 'line-beginning-position)
       (defalias 'brf-bol-position #'line-beginning-position))
      ((fboundp 'point-at-bol)
       (defalias 'brf-bol-position #'point-at-bol))
      (t ; Supply our own definition
       (defun brf-bol-position (&optional n)
	 "Return the index of the character at the start of the line."
	 (save-excursion
	   (beginning-of-line n)
	   (point)))))

;;;
;;; Transient region compatibility functions
;;;

(defun brf-region-active-p ()
  "Emacs/XEmacs compatibility function to test for an active region."
  (if brf-xemacs-flag
      zmacs-region-active-p
    mark-active))

(defun brf-transient-mark-mode (arg)
  "Emacs/XEmacs compatibility function to set transient mark mode with ARG.
Returns the previous setting."
  (if brf-xemacs-flag
      (prog1 zmacs-regions
	(setq zmacs-regions arg))
    (prog1 transient-mark-mode
      (setq transient-mark-mode arg))))

(defun brf-activate-region ()
  "Ensure region is highlit correctly in XEmacs.
This function does nothing in GNU Emacs."
  (when brf-xemacs-flag
    (if zmacs-region-active-p
	(zmacs-update-region)
      (zmacs-activate-region))))

(defun brf-deactivate-region (&optional force)
  "Ensure region is deactivated.
This function does nothing in GNU Emacs, as redisplay clears the region,
unless the optional arg FORCE is set."
  (cond (brf-xemacs-flag
	 (when zmacs-region-active-p
	   (zmacs-deactivate-region)))
	(t				; GNU Emacs
	 (when force
	   (deactivate-mark)))))

(defun brf-keep-region ()
  "Make the current command keep the region in XEmacs.
This function does nothing in GNU Emacs."
  (when brf-xemacs-flag
    (setq zmacs-region-stays t)))

(provide 'brf-compat)

;; Local Variables:
;; tab-width: 8
;; indent-tabs-mode: t
;; End:

;;; brf-compat.el ends here
