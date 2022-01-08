;;; init.el --- Emacs init file.	-*- lexical-binding: t -*-

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
;; main configure of flymacs

;;; Code:

(let* ((file-name-handler-alist nil))
                                        ; customize and function defination
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

                                        ; basic setup
  (require 'init-default)
  (require 'packages-init)
  ;; (require 'init-autoloads)
  (load (concat user-emacs-directory "lisp/init-compile.el"))
  (require 'init-performance)
  (require 'init-misc)

  (require 'ui-cnfont)
  (require 'ui-tree)
  (require 'ui-doomline)
  ;; (require 'ui-tab)
  (require 'ui-configure)
  (require 'ui-dashboard)
  (require 'ui-theme)

  (add-hook 'after-init-hook (lambda () (require 'edit-common)))
  (require 'edit-autosave)
  (auto-save-enable)
  (require 'edit-xah-fly-keys)

  
  (require 'init-ivy)
  (require 'init-hydra)
                                        ; code edit

  (require 'lang-python)
  (require 'lang-c)

                                        ; code compilation

  (require 'complete-company)
  (require 'complete-lsp)

                                        ; other modes
  (require 'init-org)
  (add-hook 'org-mode-hook (lambda () (require 'init-roam)))
  (require 'reading)
  (require 'init-git)
  (load (concat user-emacs-directory "config-user.el"))
  )
