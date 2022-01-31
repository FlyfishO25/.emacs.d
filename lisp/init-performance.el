;; init-performance.el --- Speed up emacs configure.   -*- lexical-binding: t -*-

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


;;; Commentary:
;;
;; Performance configurations.
;;


;;; Code:

;; Don't load outdated lisp files
(setq load-prefer-newer t)

;; Speed up startup
(setq auto-mode-case-fold nil)

(setq gc-cons-threshold (* 384 1024 1024)
      gc-cons-percentage 0.6)

(setq read-process-output-max (* 3 1024 1024)) ;; 3mb
(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 100000000
                  gc-cons-percentage 0.1)))
(use-package esup
  :config
  (setq esup-depth 0))

;;; init-performance ends here
