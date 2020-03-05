;;; b-compat.el --- XEmacs compatibility code for b-mode -*- lexical-binding: t -*-

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
;;  See b.el

;;; Code:

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (defconst b-xemacs-flag (featurep 'xemacs)
    "Non-nil means this version of Emacs is XEmacs."))

;; Load XEmacs overlay compatibility
(when b-xemacs-flag
  (require 'overlay))

;; Silence the byte compiler
(eval-when-compile
  (cond (b-xemacs-flag
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
       (defalias 'b-bol-position 'line-beginning-position))
      ((fboundp 'point-at-bol)
       (defalias 'b-bol-position 'point-at-bol))
      (t ; Supply our own definition
       (defun b-bol-position (&optional n)
	 "Return the index of the character at the start of the line."
	 (save-excursion
	   (beginning-of-line n)
	   (point)))))

;;;
;;; Transient region compatibility functions
;;;

(defun b-region-active-p ()
  "Emacs/XEmacs compatibility function to test for an active region."
  (if b-xemacs-flag
      zmacs-region-active-p
    mark-active))

(defun b-transient-mark-mode (arg)
  "Emacs/XEmacs compatibility function to set transient mark mode with ARG.
Returns the previous setting."
  (if b-xemacs-flag
      (prog1 zmacs-regions
	(setq zmacs-regions arg))
    (prog1 transient-mark-mode
      (setq transient-mark-mode arg))))

(defun b-activate-region ()
  "Ensure region is highlit correctly in XEmacs.
This function does nothing in GNU Emacs."
  (when b-xemacs-flag
    (if zmacs-region-active-p
	(zmacs-update-region)
      (zmacs-activate-region))))

(defun b-deactivate-region (&optional force)
  "Ensure region is deactivated.
This function does nothing in GNU Emacs, as redisplay clears the region,
unless the optional arg FORCE is set."
  (cond (b-xemacs-flag
	 (when zmacs-region-active-p
	   (zmacs-deactivate-region)))
	(t				; GNU Emacs
	 (when force
	   (deactivate-mark)))))

(defun b-keep-region ()
  "Make the current command keep the region in XEmacs.
This function does nothing in GNU Emacs."
  (when b-xemacs-flag
    (setq zmacs-region-stays t)))

(provide 'b-compat)

;;; b-compat.el ends here
