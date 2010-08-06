;; sackspace.el --- a better backspace

;; Copyright (C) 2010 by Michael Markert
;; Author: 2010 Michael Markert <markert.michael@googlemail.com>
;; Created: 2010/08/01
;; Version: 0.3
;; Time-stamp: <2010-08-07 00:18:27 cofi>

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
;; TODO: add doc

;; Homepage: http://github.com/cofi/sackspace.el
;; Git-Repository: git://github.com/cofi/sackspace.el.git

(eval-when-compile
  (require 'cl))

;; User-defined Variables ========================================
(defgroup sackspace nil
  "A better backspace."
  :tag "Sackspace"
  :link '(url-link :tag "Homepage" "http://github.com/cofi/sackspace.el")
  :prefix "sack/"
  :group 'convenience)

(defcustom sack/keys '("<backspace>"
                       "C-<backspace>"
                       "M-<backspace>"
                       "S-<backspace>")
  "Keys sackspace should install to.
Value must be a string that can be interpreted by `read-kbd-macro'.
By default Backspace, Control-Backspace, Meta-Backspace and Shift-Backspace."
  :type '(repeat :args (string)))

(defcustom sack/fun '(sack/tabstop
                      sack/word
                      sack/plain
                      sack/whitespace)
  "Functions sackspace should install.
By default `sack/tabstop', `sack/word', `sack/plain' and `sack/whitespace'."
  :type '(repeat function)
  :group 'sackspace)

(defcustom sack/backward-word (function backward-kill-word)
  "Function to use for removing a word backward."
  :type 'function
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
        (pairlis sack/keys sack/fun)))

(defun sack/install-in-emacs ()
  "Install keys appropriate for normal Emacs.
Bind selected functions to selected keys via `global-set-key'."
  (interactive)
  (mapc (lambda (pair)
          (global-set-key (read-kbd-macro (car pair)) (cdr pair)))
        (pairlis sack/keys sack/fun)))
;; ==================================================

;; Functions ========================================
(defun sack/word (&optional words)
  "Kill words."
  (interactive "p")
  (funcall sack/backward-word words))

(defun sack/plain (&optional chars)
  "Delete `chards' chars."
  (interactive "p")
  (backward-delete-char (or chars 1)))

(defun sack/plain-space (&optional chars)
  "Delete `chars' chars (untabify tabs before)."
  (interactive "p")
  (backward-delete-char-untabify (or chars 1)))

(defun sack/tabstop (&optional count)
  "Delete preceding space or chars.
Delete up to `count' times `tab-width' preceding spaces.
On preceding non-space delete up to `count' chars."
  (interactive "p")
  (let* ((start (point))
         (tab-off (mod (current-column)
                       (* count tab-width)))
         (max-back (if (= tab-off 0)
                       (* count tab-width)
                     tab-off)))
    (skip-chars-backward " " (- start max-back))
    (if (/= (point) start)
        (delete-region (point) start)
      (backward-delete-char count))))

(defun sack/whitespace (&optional always-delete)
  "Kill all whitespace before point.
Kills at least 1 char if `always-delete' is set (including non-whitespace)."
  (interactive "P")
  (let ((start (point)))
    (skip-chars-backward " \t\r\n")
    (if (/= (point) start)
        (delete-region (point) start)
      (if always-delete
          (backward-delete-char 1)))))

(provide 'sackspace)
