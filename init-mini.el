;;; init-mini.el --- Emacs mini init file.	-*- lexical-binding: t -*-

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

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq use-package-verbose t)
(setq package-enable-at-startup nil)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

(unless (eq window-system 'ns)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; ban ads
(defun display-startup-echo-area-message ()
  "Re-define this funciton to ban GNU's ad."
  (message nil))

(defun ns-auto-titlebar-set-frame (frame &rest _)
  "Set ns-appearance frame parameter for FRAME to match its background-mode parameter."
  (when (display-graphic-p frame)
    (let ((mode (frame-parameter frame 'background-mode)))
      (modify-frame-parameters frame `((ns-transparent-titlebar . t) (ns-appearance . ,mode))))))

(defun ns-auto-titlebar-set-all-frames (&rest _)
  "Set ns-appearance frame parameter for all frames to match their background-mode parameter."
  (mapc 'ns-auto-titlebar-set-frame (frame-list)))

(when (eq system-type 'darwin) (progn
                                 (add-hook 'after-init-hook 'ns-auto-titlebar-set-all-frames)
                                 (add-hook 'after-make-frame-functions 'ns-auto-titlebar-set-frame)
                                 (advice-add 'frame-set-background-mode :after 'ns-auto-titlebar-set-frame)
                                 ))

(setq flymacs--file-name-handler-alist-old file-name-handler-alist)

(let* ((file-name-handler-alist nil))
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))

					; basic setup
  (require 'init-funcs)
  (require 'init-const)
  
  (loadpkg 'init-performance t)
  (loadpkg 'init-option t)
  (loadpkg 'packages-init t)
  (loadpkg 'init-server)
  (loadpkg 'init-compile t)
  (loadpkg 'init-misc t)    
  
  (require 'option-handler)

  (flymacs-handle-minimal-startup)
  
  (loadpkg 'init-user t)  
  )

(setq file-name-handler-alist flymacs--file-name-handler-alist-old)

;;; init.el ends here
