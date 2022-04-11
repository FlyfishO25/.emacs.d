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
  "Load customization file NAME if MUST-BE-LOADED is t.
Otherwise, if it is not currently in the git merge state, load it."
  (when (or (not (flymacs-git-merge-p)) must-be-loaded)
    (load (file-truename (concat user-emacs-directory (format "lisp/%s" name))) t t)))

(defun flymacs-post-install ()
  "Install programs after Emacs startup."
  (if (not (eq system-type "windows-nt"))
      (progn
        (when (not (executable-find "rg"))
          (system-packages-install "ripgrep"))
        (unless (or (executable-find "gls")
                    (not (eq system-type 'darwin)))
          (system-packages-install "coreutils")))
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
  (if (file-exists-p file)
      (if (file-newer-than-file-p
           (file-truename file)
           (file-truename (concat (file-name-directory file) (concat (car (split-string (file-name-nondirectory file) "\\.")) ".elc"))))
          (byte-compile-init-files file))
    ))

(defun icons-displayable-p ()
  "Return non-nil if `all-the-icons' is displayable."
  (and display-icon
       (or (display-graphic-p) (daemonp))
       (eq flymacs-ui 'rich)
       (require 'all-the-icons nil t)))

(defmacro use-package-hook! (package when &rest body)
  "Reconfigures a package's `use-package!' block.
This macro must be used *before* PACKAGE's `use-package!' block. Often, this
means using it from your DOOMDIR/init.el.
Under the hood, this uses use-package's `use-package-inject-hooks'.
PACKAGE is a symbol; the package's name.
WHEN should be one of the following:
  :pre-init :post-init :pre-config :post-config
WARNINGS:
- The use of this macro is more often than not a code smell. Use it as last
  resort. There is almost always a better alternative.
- If you are using this solely for :post-config, stop! `after!' is much better.
- If :pre-init or :pre-config hooks return nil, the original `use-package!''s
  :init/:config block (respectively) is overwritten, so remember to have them
  return non-nil (or exploit that to overwrite Doom's config)."
  ;; This macro is from doom-emacs @see https://github.com/hlissner/doom-emacs/blob/f73ae8eee176b46fc8d02d8702d2da9bc25b3472/core/core-modules.el#L528
  (declare (indent defun))
  (unless (memq when '(:pre-init :post-init :pre-config :post-config))
    (error "'%s' isn't a valid hook for use-package-hook!" when))
  `(progn
     (setq use-package-inject-hooks t)
     (add-hook ',(intern (format "use-package--%s--%s-hook"
                                 package
                                 (substring (symbol-name when) 1)))
               (lambda () ,@body)
               'append)))


;; Update
;; function fron https://github.com/seagle0128/.emacs.d/blob/754eb554ca2dd22807898bd5a4257a57f6ab5cfd/lisp/init-funcs.el#L426
(defun update-config ()
  "Update Centaur Emacs configurations to the latest version."
  (interactive)
  (let ((temp-dir (expand-file-name user-emacs-directory)))
    (if (file-exists-p temp-dir)
        (progn
          (message "Updating configurations...")
          (cd temp-dir)
          (shell-command "git pull")
          (shell-command "git submodule update --remote")
          (message "Updating configurations...done"))
      (message "\"%s\" doesn't exist" temp-dir))))

(defalias 'update-packages #'auto-package-update-now)

(provide 'init-funcs)

;;; init-funcs.el ends here
