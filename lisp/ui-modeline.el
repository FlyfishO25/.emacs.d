;;; ui-modeline.el --- setup modeline -*- lexical-binding: t; -*-

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup modeline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
(use-package doom-modeline
      :ensure t
      :defer t
      :hook (after-init . doom-modeline-mode)
      :init
      (setq doom-modeline-minor-modes t)
      (setq doom-modeline-height 30)
      :config
      (use-package sky-color-clock
        :load-path "site-lisp/"
        :if (boundp 'flymacs-latitude)
        :demand
        :commands (sky-color-clock-initialize)
        :config
        (sky-color-clock-initialize flymacs-latitude)
        (push '(:eval (sky-color-clock)) (default-value 'mode-line-misc-info))
        (setq sky-color-clock-format "%m/%d %H:%M")
        (setq sky-color-clock-enable-emoji-icon nil))
)

(when (>= emacs-major-version 25.2)
  (use-package minions
    :hook (doom-modeline-mode . minions-mode)))

;;; ui-modeline.el ends here.
