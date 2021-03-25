;;; brf-menu.el --- Easily create buffer-oriented menus -*- lexical-binding: t -*-

;; Copyright (C) 1999-2021 Mike Woolley
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

;; Easily create buffer-oriented menus.
;; Inspired by the old package `generic-menu'.

;;; Code:

(require 'brf-compat)
(eval-when-compile (require 'cl-lib))

(defvar-local brf-menu-parameters-alist nil
  "Menu parameters.")

(defvar brf-menu-parameters-default-alist
  '((:header-line . "Make a selection:")
    (:buffer-name . "*Menu*")
    (:mode-name .  "Brf Menu")
    (:display-function . (lambda (item) (prin1-to-string item t)))
    (:truncate-lines . t))
  "Default Menu parameters.")

(defun brf-menu-parameter (name &optional params)
  "Return parameter NAME.
If PARAMS is non-nil, use that instead of `brf-menu-parameters-alist'."
  (let ((pair (assoc name (or params brf-menu-parameters-alist))))
    (cdr pair)))

(defvar brf-menu-mode-map
  ;; Default keymap
  (let ((map (make-sparse-keymap)))
    (define-key map " "       'brf-menu-select)
    (define-key map "\C-m"    'brf-menu-select)
    (define-key map [up]      'brf-menu-up)
    (define-key map [down]    'brf-menu-down)
    (define-key map "p"       'brf-menu-up)
    (define-key map "n"       'brf-menu-down)
    (define-key map "g"       'brf-menu-refresh)
    (define-key map "?"       'brf-menu-help)
    (define-key map "\C-g"    'brf-menu-abort)
    (define-key map "q"       'brf-menu-quit)
    map)
  "Keymap for `brf-menu-mode'.")

;; Define a new Major Mode for the menu
(define-derived-mode brf-menu-mode
  special-mode	; Parent mode
  nil		; Mode name (will be set later)
  nil		; Docstring (use default)
  :syntax-table nil
  :abbrev-table nil)

;;;###autoload
(defun brf-menu (&rest plist)
  "Show a menu defined by the keywords supplied in PLIST.

Available keywords:

:items SEQUENCE
	The Menu items.
:display-function FUNCTION
	Function called to display an item.
:select-function  FUNCTION
	Function called when the user selects an item.
:regexp-start-position REGEXP
	Regexp to set the initial cursor position.
:goto-start-position-function FUNCTION
	Function called to set the initial cursor position.
	Takes precedence over the regexp.
:mode-help STRING
	Help string to show at the top of the help window for the
	menu.
:quit-function FUNCTION
	Function called if the user quits the menu.
:header-line STRING
	Header line.
:font-lock-keywords
	Font-lock definition.
:truncate-lines BOOLEAN
	Truncate menu lines.
:mode-name STRING
	Name of menu mode.
:buffer-name STRING
	Name of menu buffer.
:keymap KEYMAP
	Additional key bindings for the menu buffer.
:max-items INTEGER
	Total number of items allowed in the menu."

  ;; Append the default parameter alist to the supplied parameters
  ;; Also save the current window configuration to the parameters
  (let ((params (append (list (cons :window-config
				    (current-window-configuration)))
			(brf-menu-plist-to-alist plist)
			brf-menu-parameters-default-alist)))
    ;; Create the menu buffer
    (let ((buffer (get-buffer-create
		   (brf-menu-parameter :buffer-name params))))
      (set-buffer buffer)

      ;; Turn on `brf-menu-mode'
      (brf-menu-mode)

      ;; Save the params into the buffer-local variable
      ;; This needs to go after setting the Major Mode because it clears all local variables
      (setq brf-menu-parameters-alist params)

      ;; Merge the supplied keymap and the major mode's keymap
      (let ((keymap (brf-menu-parameter :keymap)))
	(when keymap
	  (require 'derived)
	  (derived-mode-merge-keymaps brf-menu-mode-map keymap)
	  (setq brf-menu-mode-map keymap)
	  (use-local-map brf-menu-mode-map)))

      ;; Apply the supplied font-lock
      (when (brf-menu-parameter :font-lock-keywords)
	(make-local-variable 'font-lock-defaults)
	(make-local-variable 'font-lock-verbose)
	(setq font-lock-defaults (list (brf-menu-parameter :font-lock-keywords) t)
	      font-lock-verbose nil))

      ;; Set the rest of the menu parameters
      (setq truncate-lines (brf-menu-parameter :truncate-lines)
	    mode-name (brf-menu-parameter :mode-name))

      ;; Display the menu buffer
      (brf-menu-display)
      (pop-to-buffer buffer)
      (shrink-window-if-larger-than-buffer))))

;; Indent uses of `brf-menu' like a defun
(put 'brf-menu 'lisp-indent-function 'defun)

(defun brf-menu-plist-to-alist (plist)
  "Return PLIST as an alist."
  (let ((alist nil))
    (while plist
      (setq alist (cons (cons (car plist)
			      (cadr plist))
			alist))
      (setq plist (cddr plist)))
    alist))

(defun brf-menu-num-items ()
  "Return the number of items in the menu."
  (let ((num-items (length (brf-menu-parameter :items)))
	(max-items (brf-menu-parameter :max-items)))
    (if max-items
	(min max-items num-items)
      num-items)))

(defun brf-menu-current-item ()
  "Return the current menu item."
  (let ((line (count-lines (point-min)
			   (brf-bol-position))))
    (if (and (>= line 1) ; ie the header line
	     (<= line (brf-menu-num-items)))
	(nth (1- line) (brf-menu-parameter :items))
      (user-error "You aren't on a valid line"))))

(defun brf-menu-goto-start-position ()
  "Set the initial position of point in the menu.
This position is defined by the parameter
`:goto-start-position-function', a function called without any
arguments or `:regexp-start-position', a regular expression.
`:goto-start-position-function' takes precedence."
  (interactive)
  (goto-char (point-min))
  (let ((fun (brf-menu-parameter :goto-start-position-function))
	(goto-regexp (brf-menu-parameter :regexp-start-position)))
    (if fun
	(funcall fun)
      (if goto-regexp
	  (if (search-forward-regexp goto-regexp nil t)
	      (goto-char (match-beginning 0)))
	(forward-line 1)))))

(defun brf-menu-display ()
  "Fill the current buffer with the header line and menu items."
  (let ((header (brf-menu-parameter :header-line))
	(items (brf-menu-parameter :items))
	(insert-fun (brf-menu-parameter :display-function))
	(inhibit-read-only t))
    (erase-buffer)
    (insert header "\n")

    ;; Insert each item on a new line, up to the maximum number
    (cl-loop for item in items
	     for i from 1 to (brf-menu-num-items)
	     do (insert (funcall insert-fun item) "\n"))
    (backward-delete-char 1)

    (when (brf-menu-parameter :font-lock-keywords)
      (font-lock-fontify-region (point-min) (point-max)))
    (set-buffer-modified-p nil))

  (brf-menu-goto-start-position))

(defun brf-menu-refresh ()
  "Redisplay the menu."
  (interactive)
  (brf-menu-display))

(defun brf-menu-select ()
  "User selected the current item in the menu."
  (interactive)
  (let ((select-fun (brf-menu-parameter :select-function)))
    (if select-fun
	(funcall select-fun (brf-menu-current-item))
      (user-error "No select action defined"))))

(defun brf-menu-up (arg)
  "Move cursor up ARG lines in the menu."
  (interactive "p")
  (brf-menu-down (- arg)))

(defun brf-menu-down (arg)
  "Move cursor down ARG lines in the menu."
  (interactive "p")
  (let* ((lines (1+ (brf-menu-num-items)))
	 (curr-line (count-lines (point-min) (brf-bol-position)))
	 (new-line (mod (+ curr-line arg) lines)))
    (when (= new-line 0)
      (setq new-line (if (>= arg 0) 1 (1- lines))))
    (forward-line (- new-line curr-line))))

(defun brf-menu-quit ()
  "Exit the menu."
  (interactive)

  ;; Run the user's Quit function
  (let ((quit-fun (brf-menu-parameter :quit-function)))
    (when quit-fun
      (funcall quit-fun)))

  ;; Close the menu window
  (bury-buffer (current-buffer))
  (set-window-configuration (brf-menu-parameter :window-config)))

(defun brf-menu-abort ()
  "Abort the menu."
  (interactive)
  (ding)
  (bury-buffer (current-buffer))
  (set-window-configuration (brf-menu-parameter :window-config)))

(defun brf-menu-help ()
  "Show help for the menu."
  (interactive)
  (describe-function 'brf-menu-mode)
  (let ((helpstring (brf-menu-parameter :mode-help)))
    (when helpstring
      (with-current-buffer "*Help*"
	(let ((inhibit-read-only t))
	  (save-excursion
	    (goto-char (point-min))
	    (insert helpstring "\n\n")))))))

(provide 'brf-menu)

;; Local Variables:
;; tab-width: 8
;; indent-tabs-mode: t
;; End:

;;; brf-menu.el ends here
