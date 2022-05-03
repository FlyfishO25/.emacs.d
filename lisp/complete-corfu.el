;; complete-corfu.el --- Initialize corfu configurations.	-*- lexical-binding: t -*-

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
;;
;; Auto-completion configurations.
;;

;;; Code:

(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (tab-always-indent 'complete)

  ;; Enable Corfu only for certain modes.
  :hook (prog-mode . corfu-mode)
  :commands (global-corfu-mode)
  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :bind (:map corfu-map ("C-M-SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode)
  :config
  
  (use-package kind-icon
    :demand
    :custom
    (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
    :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
  
  (use-package corfu-doc
    :after corfu
    :hook (corfu-mode-hook . corfu-doc-mode)
    :config
    (define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down) ;; corfu-next
    (define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)  ;; corfu-previous
    (define-key corfu-map (kbd "M-d") #'corfu-doc-toggle)
    )
  )

(use-package yasnippet
  :diminish yas-minor-mode
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode)))

(use-package yasnippet-snippets
  :after yasnippet)
;;; complete-corfu.el ends here
