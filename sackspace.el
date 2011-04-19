;; sackspace.el --- a better backspace

;; Copyright (C) 2010 by Michael Markert
;; Author: 2010 Michael Markert <markert.michael@googlemail.com>
;; Created: 2010/08/01
;; Version: 0.4.4

;; Keywords: delete

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Known Bugs:
;; See http://github.com/cofi/sackspace.el/issues

;; Install:
;; Just load file and run `sack/install-in-viper' or `sack/install-in-emacs'.
;; All `sack' functions are available for use as well.
;;
;; Usage:
;; See readme.

;; Homepage: http://github.com/cofi/sackspace.el
;; Git-Repository: git://github.com/cofi/sackspace.el.git

;; User-defined Variables ========================================
(defgroup sackspace nil
  "A better backspace."
  :tag "Sackspace"
  :link '(url-link :tag "Homepage" "http://github.com/cofi/sackspace.el")
  :prefix "sack/"
  :group 'convenience)

(defcustom sack/key-bindings '(("<backspace>" . sack/tabstop)
                               ("C-<backspace>" . sack/word)
                               ("M-<backspace>" . sack/plain)
                               ("S-<backspace>" . sack/whitespace))
  "Keybindings sackspace should install.
Keys must be strings that can be interpreted by `read-kbd-macro'."
  :type '(repeat (cons :tag "Keybinding" string (choice (function-item sack/tabstop)
                                                        (function-item sack/word)
                                                        (function-item sack/plain)
                                                        (function-item sack/plain-space)
                                                        (function-item sack/whitespace))))
  :group 'sackspace)

(defcustom sack/backward-word (function backward-kill-word)
  "Function to use for removing a word backward."
  :type 'function
  :group 'sackspace)

(defcustom sack/honor-subword t
  "If sackspace should follow `subword-mode'."
  :type 'boolean
  :group 'sackspace)

(defcustom sack/honor-autopair t
  "If sackspace should follow `autopair-mode'"
  :type 'boolean
  :group 'sackspace)

(defcustom sack/honor-paredit t
  "If sackspace should follow `paredit-mode'"
  :type 'boolean
  :group 'sackspace)

(defcustom sack/force-viper-install nil
  "Install viper-keys even if `viper-vi-style-in-minibuffer' is non-nil.
WARNING: This maybe leads to unwanted behavior."
  :type 'boolean
  :group 'sackspace)
;; ==================================================

;; Installer ========================================
(defun sack/install-in-viper ()
  "Install keys appropriate for viper.
Binds selected functions to selected keys in `viper-insert-global-user-map'."
  (interactive)
  (if (and viper-vi-style-in-minibuffer
           (not sack/force-viper-install))
    (error "Refuse to install keys, because it could lead to unwanted behavior.\
Set `sack/force-viper-install' to non-nil if you want it nevertheless."))
  (mapc (lambda (pair)
          (define-key viper-insert-global-user-map (read-kbd-macro (car pair))
                                                   (cdr pair)))
        sack/key-bindings))

(defun sack/install-in-emacs ()
  "Install keys appropriate for normal Emacs.
Bind selected functions to selected keys via `global-set-key'."
  (interactive)
  (mapc (lambda (pair)
          (global-set-key (read-kbd-macro (car pair)) (cdr pair)))
        sack/key-bindings))
;; ==================================================

;; Functions ========================================
(defun sack/word (&optional words)
  "Kill words.
Honors subword-mode (if enabled)."
  (interactive "p")
  (if (and sack/honor-subword (bound-and-true-p subword-mode))
      (subword-backward-kill words)
    (funcall sack/backward-word words)))

(defun sack/plain (&optional chars)
  "Delete `chars' chars.
Honors autopair (if enabled)."
  (interactive "p")
  (unless (sack/autopair-backspace)
    (backward-delete-char (or chars 1))))

(defun sack/plain-space (&optional chars)
  "Delete `chars' chars (untabify tabs before).
Honors autopair (if enabled)."
  (interactive "p")
  (unless (sack/autopair-backspace)
    (backward-delete-char-untabify (or chars 1))))

(defun sack/tabstop (&optional count)
  "Delete preceding space or chars.
Delete up to `count' times `tab-width' preceding spaces.
On preceding non-space delete up to `count' chars.
Honors paredit (if enabled) and autopair (if enabled) in that order."
  (interactive "p")
  (unless (or (sack/paredit-backspace count)
              (sack/autopair-backspace))
    (let* ((start (point))
           (tab-off (mod (current-column)
                         (* count tab-width)))
           (max-back (if (= tab-off 0)
                         (* count tab-width)
                       tab-off)))
      (skip-chars-backward " " (- start max-back))
      (if (/= (point) start)
          (delete-region (point) start)
        (backward-delete-char count)))))

(defun sack/whitespace (&optional cross-line)
  "Kill all whitespace -- except end of lines -- before point.
Also kills end of lines when called with prefix."
  (interactive "P")
  (let ((start (point))
        (whitespace (if cross-line
                        " \t\r\n"
                      " \t")))
    (skip-chars-backward whitespace)
    (if (/= (point) start)
        (delete-region (point) start))))

(defun sack/autopair-backspace ()
  "Emulates `autopair-backspace'.
Takes action only if `sack/honor-autopair' is non-nil."
  (when (and sack/honor-autopair (bound-and-true-p autopair-mode)
           (autopair-find-pair (char-before)))
    (setq autopair-action (list 'backspace (autopair-find-pair (char-before)) (point)))
    (autopair-default-handle-action 'backspace (autopair-find-pair (char-before)) (point))
    (backward-delete-char 1)
    t))                                 ; need to signal fun was successful 

(defun sack/paredit-backspace (&optional count)
  "Call `paredit-backward-delete' if we honor paredit."
  (when (and sack/honor-paredit (bound-and-true-p paredit-mode))
    (paredit-backward-delete count)
    t))                                 ; need to signal fun was successful 

(provide 'sackspace)
;; sackspace.el ends here

;; Changelog
;; 0.4.4: Added support for `paredit'.
;; 0.4.3.1: Fixed `autopair-backspace'.
;; 0.4.3: Added support for `autopair'.
;; 0.4.2: Added support for `subword-mode'.
