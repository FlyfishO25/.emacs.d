;;; ui-tbeme.el --- setup themes -*- lexical-binding: t; -*-

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
;; Setup themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package doom-themes
  :custom-face
  (doom-modeline-buffer-file ((t (:inherit (mode-line bold)))))
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs usersXb
  (setq doom-themes-treemacs-theme "doom-colors")
  (with-eval-after-load 'lsp-treemacs
    (doom-themes-treemacs-config))
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

(unless (boundp 'flymacs-theme-auto)
  (setq flymacs-theme-auto '(("7:00" . doom-one-light)
                             ("17:30" . doom-vibrant))))

(if (listp flymacs-theme-auto)
    (progn
      (use-package circadian
        :hook ('after-make-frame-functions . (lambda ()
                                               (circadian-setup)))
        :config
          (setq circadian-themes flymacs-theme-auto))
      (circadian-setup))
  (load-theme flymacs-theme-auto t))

;;; ui-themes.el ends here.
