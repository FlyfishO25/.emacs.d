;;; lang-c.el --- C/C++ init file. -*- lexical-binding: t -*-

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
;; Configures of C/C++ mode.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map
              ("C-c c" . compile))
  :hook (c-mode-common . (lambda () (c-set-style "")))
  :config
  (use-package modern-cpp-font-lock
    :diminish
    :init (modern-c++-font-lock-global-mode t))
  (use-package clang-format
    :ensure t
    :defer t
    :bind (("C-c C-f" . clang-format-region))
    ;; clang-format -style=google -dump-config > .clang-format to generate config file.
    )
  (use-package google-c-style
    :if flymacs-cpp-google-style
    :config
    (add-hook 'c-mode-common-hook 'google-set-c-style)
    (add-hook 'c-mode-common-hook 'google-make-newline-indent))
  (setq compile-command my:compile-command)
  ;; Change tab key behavior to insert spaces instead
  (setq-default indent-tabs-mode nil)
  ;; Enable hide/show of code blocks
  (add-hook 'c-mode-common-hook 'hs-minor-mode)
  ;; Set the number of spaces that the tab key inserts (usually 2 or 4)
  (setq c-basic-offset 4)
  ;; Set the size that a tab CHARACTER is interpreted as
  ;; (unnecessary if there are no tab characters in the file!)
  (setq tab-width 4)
  )

;;; lang-c.el ends here
