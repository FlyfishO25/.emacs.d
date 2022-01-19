;;; init-funcs.el --- provide functions	-*- lexical-binding: t -*-

;; Copyright (C) 2022 FlyfishO25

;; Author: FlyfishO25 <markzhou0125@gmail.com>
;; URL: https://github.com/FlyfishO25/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;; Functions used in flymacs

;;; Code:

(defun flymacs-git-merge-p ()
  "Return non-nil if using flymacs to edit git merge message."
  (boundp 'startup-now))

(defun loadpkg (name &optional must-be-loaded)
  "Load customization files"
  (when (or (not (flymacs-git-merge-p)) must-be-loaded)
    (load (file-truename (concat user-emacs-directory (format "lisp/%s" name))) t t)))

(defun flymacs-post-install ()
  (if (not (eq system-type "windows-nt"))
    (when (not (executable-find "rg"))
      (system-packages-install "ripgrep"))
    (message "Cannot auto install ripgrep executable (rg), please install it."))
)

(defun byte-compile-init-files (file)
  "Automatically compile FILE."
  (interactive)
  (save-restriction
    ;; Suppress the warning when you setq an undefined variable.
    (if (>= emacs-major-version 23)
        (setq byte-compile-warnings '(not free-vars obsolete cl-function))
      (setq byte-compile-warnings
            '(unresolved
              callargs
              redefine
              obsolete
              noruntime
              cl-warnings
              interactive-only)))
    (byte-compile-file (expand-file-name file)))
  )


(defun flymacs-compile-file (file)
  "Byte compile FILE, you should make sure that FILE is a .el file."
  (if (file-newer-than-file-p
       (file-truename file)
       (file-truename (concat (file-name-directory file) (concat (car (split-string (file-name-nondirectory file) "\\.")) ".elc"))))
      (byte-compile-init-files file))
  )

(defun icons-displayable-p ()
  "Return non-nil if `all-the-icons' is displayable."
  (and display-icon
       (display-graphic-p)
       (require 'all-the-icons nil t)))

(defun flymacs-setup ()
  )

(provide 'init-funcs)

;;; init-funcs.el ends here
