;;; init-compile.el --- provide compile feature	-*- lexical-binding: t -*-

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
;; Provide compile feature to speed up flymacs.

;;; Code:

(require 'init-funcs)

(loadpkg 'init-const)

(use-package exec-path-from-shell
  :ensure t
  :config
  (if (and (fboundp 'native-comp-available-p)
                (native-comp-available-p))
      (progn
        (message "Native comp is available")
        ;; Using Emacs.app/Contents/MacOS/bin since it was compiled with
        ;; ./configure --prefix="$PWD/nextstep/Emacs.app/Contents/MacOS"
        ;; Append to path to give priority to values from exec-path-from-shell-initialize.
        (add-to-list 'exec-path (concat invocation-directory (file-name-as-directory "bin")) t)
        (setenv "LIBRARY_PATH" (concat (getenv "LIBRARY_PATH")
                                       (when (getenv "LIBRARY_PATH")
                                         ":")
                                       ;; This is where Homebrew puts libgccjit libraries.
                                       (car (file-expand-wildcards
                                             ;; (expand-file-name "/opt/homebrew/opt/libgccjit/lib/gcc/*")
                                             (expand-file-name "/usr/local/Cellar/libgccjit/11.2.0_1/lib/gcc/11/")
                                             ))))
        ;; Only set after LIBRARY_PATH can find gcc libraries.
        (defvar comp-deferred-compilation t)
        (setq package-native-compile t)
        (defvar comp-speed 3))
    (message "Native comp is *not* available")))

(setq exec-path-from-shell-arguments (list "-l"))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically compile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-hook
;;  'after-save-hook
;;  (function
;;   (lambda ()
;;     (if (string= (file-truename "~/.emacs.d/init.el")
;;                  (file-truename (buffer-file-name)))
;;         (byte-compile-init-files (file-truename "~/.emacs.d/init.el")))
;;     )
;;   )
;;  )

(while flymacs-files-to-compile
  (flymacs-compile-file (eval (pop flymacs-files-to-compile)))
  )

;;; init-compile.el ends here
