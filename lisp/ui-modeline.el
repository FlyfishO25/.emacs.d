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
      :if (eq flymacs-ui 'rich)
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
    :if (eq flymacs-ui 'rich)
    :hook (doom-modeline-mode . minions-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Powerline theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; powerline theme where the modes are on the right side.

(use-package powerline
  :ensure t
  :if (eq flymacs-ui 'simple)
  :demand
  :config
  ;; fix window algorithm problems in powerline. (by doom-modeline)
  (defun my/pl--get-current-window (&optional frame)
    "Get the current window but should exclude the child windows."
    (if (and (fboundp 'frame-parent) (frame-parent frame))
        (frame-selected-window (frame-parent frame))
      (frame-selected-window frame)))

  (defvar my/pl-current-window (my/pl--get-current-window))

  (defun my/pl--active ()
    "Whether is an active window."
    (unless (and (bound-and-true-p mini-frame-frame)
                 (and (frame-live-p mini-frame-frame)
                      (frame-visible-p mini-frame-frame)))
      (and my/pl-current-window
           (eq (my/pl--get-current-window) my/pl-current-window))))

  (defun my/pl-set-selected-window (&rest _)
    "Set `my/pl-current-window' appropriately."
    (let ((win (my/pl--get-current-window)))
      (setq my/pl-current-window
            (if (minibuffer-window-active-p win)
                (minibuffer-selected-window)
              win))))

  (defun my/pl-unset-selected-window ()
    "Unset `my/pl-current-window' appropriately."
    (setq my/pl-current-window nil))

  (add-hook 'pre-redisplay-functions #'my/pl-set-selected-window)

  (defun powerline-right-theme ()
    "Setup a mode-line with major and minor modes on the right side."
    (interactive)
    (setq-default mode-line-format
                  '("%e"
                    (:eval
                     (let* ((active (my/pl--active))
                            (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                            (mode-line (if active 'mode-line 'mode-line-inactive))
                            (face0 (if active 'powerline-active0 'powerline-inactive0))
                            (face1 (if active 'powerline-active1 'powerline-inactive1))
                            (face2 (if active 'powerline-active2 'powerline-inactive2))
                            (separator-left (intern (format "powerline-%s-%s"
                                                            (powerline-current-separator)
                                                            (car powerline-default-separator-dir))))
                            (separator-right (intern (format "powerline-%s-%s"
                                                             (powerline-current-separator)
                                                             (cdr powerline-default-separator-dir))))
                            (lhs (list (powerline-raw "%*" face0 'l)
                                       (powerline-raw flymacs--xah-status face0 'l)
                                       (powerline-buffer-size face0 'l)
                                       (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
                                       (powerline-raw " ")
                                       (funcall separator-left face0 face1)
                                       (powerline-narrow face1 'l)
                                       (powerline-vc face1)))
                            (center (list (powerline-raw global-mode-string face1 'r)
                                          (powerline-raw "%4l" face1 'r)
                                          (powerline-raw ":" face1)
                                          (powerline-raw "%3c" face1 'r)
                                          (funcall separator-right face1 face0)
                                          (powerline-raw " ")
                                          (powerline-raw "%6p" face0 'r)
                                          (powerline-hud face2 face1)
                                          ))
                            (rhs (list (powerline-raw " " face1)
                                       (funcall separator-left face1 face2)
                                       (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                         (powerline-raw erc-modified-channels-object face2 'l))
                                       (powerline-major-mode face2 'l)
                                       (powerline-process face2)
                                       (powerline-raw " :" face2)
                                       (powerline-raw " " face2)
                                       (funcall separator-right face2 face1)
                                       ))
                            )
                       (concat (powerline-render lhs)
                               (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                               (powerline-render center)
                               (powerline-fill face1 (powerline-width rhs))
                               (powerline-render rhs)))))))
  (powerline-right-theme)
  )

;;; ui-modeline.el ends here.
