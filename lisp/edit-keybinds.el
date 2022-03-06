;;; edit-keybinds.el --- setup keybinds for emacs -*- lexical-binding: t; -*-

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
     :load-path (lambda ()
                  (if (file-exists-p (expand-file-name "site-lisp/xah-fly-keys/xah-fly-keys.el" user-emacs-directory))
                      "site-lisp/xah-fly-keys"
                    "site-lisp/xah-fly-keys.el"))
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
       (define-key xah-fly-command-map (kbd "M-<SPC>") nil)
       (setq flymacs--xah-status "Cmd")
       ;; more here
       )

     (defun my-config-xah-fly-key-insert ()
       "Modify keys for xah fly key command mode keys
to be added to `xah-fly-insert-mode-activate-hook'"
       (interactive)
       (define-key xah-fly-insert-map (kbd "M-<SPC>") 'xah-fly-command-mode-activate)
       (setq flymacs--xah-status "Ins")
       ;; more here
       )

     (add-hook 'xah-fly-command-mode-activate-hook 'my-config-xah-fly-key-command)
     (add-hook 'xah-fly-insert-mode-activate-hook 'my-config-xah-fly-key-insert)
     
     (xah-fly-keys 1)
     )
   )
  ('evil
   (use-package evil
     :diminish undo-tree-mode
     :init
     (setq evil-want-C-u-scroll t
           evil-want-keybinding nil
           evil-shift-width 4
           evil-undo-system 'undo-tree)
     :hook (after-init . evil-mode)
     :preface
     (defun save-and-kill-this-buffer ()
       (interactive)
       (save-buffer)
       (kill-this-buffer))
     :config
     (with-eval-after-load 'evil-maps ; avoid conflict with company tooltip selection
       (define-key evil-insert-state-map (kbd "C-n") nil)
       (define-key evil-insert-state-map (kbd "C-p") nil))
     (evil-ex-define-cmd "q" #'kill-this-buffer)
     (evil-ex-define-cmd "wq" #'save-and-kill-this-buffer)
     (setq dashboard-banner-logo-title "FLYMACS, but evil")
     (use-package evil-collection
       :after evil
       :config
       (setq evil-collection-company-use-tng nil)
       (evil-collection-init))
     (use-package evil-commentary
       :after evil
       :diminish
       :config (evil-commentary-mode +1))
     )
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
;; Set default C-x C-b to ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c M-w") 'osx-copy)
(global-set-key (kbd "C-c C-y") 'osx-paste)

;;; edit-keybinds.el ends here


