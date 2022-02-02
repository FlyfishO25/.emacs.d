;;; edit-keybinds.el --- setup keybinds for emacs

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
;; Setup xah-fly-keys, evil and Emacs original keybind.
;;

;;; Code:

(require 'init-funcs)

(unless (boundp 'flymacs-keybinding)
  (progn
    (setq flymacs-keybinding 'emacs)
    (message "Variable flymacs-keybinding is not set, use 'emacs as default.")))

(pcase flymacs-keybinding
  ('xah
   (use-package xah-fly-keys
     :load-path "site-lisp/"
     :demand
     :commands (xah-fly-keys-set-layout
                xah-fly-keys)
     :init (setq xah-fly-use-meta-key nil
                 xah-fly-use-control-key nil)
     :config
     ;; (eval-and-compile (require 'xah-fly-keys))
     (xah-fly-keys-set-layout "qwerty")
     (defun my-config-xah-fly-key-command ()
       "Modify keys for xah fly key command mode keys
to be added to `xah-fly-command-mode-activate-hook'"
       (interactive)
       (define-key xah-fly-command-map (kbd "n") 'swiper)
       (define-key xah-fly-command-map (kbd "2") 'delete-window)
       ;; more here
       )

     (defun my-config-xah-fly-key-insert ()
       "Modify keys for xah fly key command mode keys
to be added to `xah-fly-insert-mode-activate-hook'"
       (interactive)
       (define-key xah-fly-insert-map (kbd "M-<SPC>") 'xah-fly-command-mode-activate)
       ;; more here
       )

     (add-hook 'xah-fly-command-mode-activate-hook 'my-config-xah-fly-key-command)
     (add-hook 'xah-fly-insert-mode-activate-hook 'my-config-xah-fly-key-insert)
     
     (xah-fly-keys 1)
     )
   )
  ('evil
   (use-package evil
     :demand
     :commands (evil-mode)
     :config
     (evil-mode 1)
     (use-package evil-collection
       :demand
       :commands (evil-collection-init)
       :config
       (evil-collection-init))
     (use-package evil-surround
       :demand
       :commands (global-evil-surround-mode)
       :config
       (global-evil-surround-mode 1)))
   )
  ('emacs
   (message "Using default keybinding."))
  )

(global-set-key (kbd "C-c C-k") 'kill-compilation)
(global-set-key (kbd "C-c c") 'comment-region)
;; Global Keyboard Shortcuts
;; Easy undo key
(global-set-key (kbd "C-/") 'undo)
;; Comment or uncomment the region
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
;; Indent after a newline, if required by syntax of language
(global-set-key (kbd "C-m") 'newline-and-indent)
;; Load the compile command
(global-set-key (kbd "C-c C-c") 'compile)
;; Undo, basically C-x u
(global-set-key (kbd "C-/") 'undo)
;; Find file in project
(global-set-key (kbd "C-x M-f") 'project-find-file)
(global-set-key (kbd "C-r") 'replace-string)

;;; edit-keybinds.el ends here

