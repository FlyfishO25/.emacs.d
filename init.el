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

                                        ; customize and function defination

;; (defvar performence-test nil)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

                                        ; basic setup
(require 'init-default)
(require 'packages-init)
(require 'init-autoloads)
(require 'init-compile)
(require 'init-performance)
(require 'init-misc)

(require 'ui-cnfont)
(require 'ui-tree)
(require 'ui-doomline)
;; (require 'ui-tab)
(require 'ui-configure)
(require 'ui-dashboard)
(require 'ui-theme)

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(add-hook 'after-init-hook (lambda () (require 'edit-common)))
(require 'edit-autosave)
(auto-save-enable)
(require 'edit-xah-fly-keys)

                                        
(require 'init-ivy)
(require 'init-hydra)
(require 'init-origami)
                                        ; code edit

(require 'lang-python)
(require 'lang-c)

                                        ; code compilation

(require 'complete-company)
(require 'complete-lsp)

                                        ; other modes
(require 'init-org)
(add-hook 'org-mode-hook (lambda () (require 'init-roam)))
(require 'reading-nov)
(require 'init-git)
(load-file (concat user-emacs-directory "config-user.el"))
