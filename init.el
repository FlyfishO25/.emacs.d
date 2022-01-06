;; -*- lexical-binding: t; -*-
;;; Code:

                                        ; customize and function defination

(defvar display-icon t)
(defvar centaur-completion-style 'childframe)
(defvar centaur-lsp 'lsp-mode)
(setq my:compile-command "~/tools/cpp11 ")
;; (defvar performence-test nil)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

                                        ; basic setup
;; Don't load outdated lisp files
(setq load-prefer-newer t)

;; Speed up startup
(setq auto-mode-case-fold nil)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(setq read-process-output-max (* 1024 1024)) ;; 1mb
(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 100000000
                  gc-cons-percentage 0.1)))

(require 'proxy-config)
(require 'packages-init)
;; (require 'init-autoloads)
(require 'init-compile)

(require 'ui-cnfont)
(require 'ui-tree)
(require 'ui-doomline)
;; (require 'ui-tab)
(require 'init-hydra)
(require 'ui-configure)
(require 'ui-dashboard)
(require 'ui-theme)

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(add-hook 'after-init-hook (lambda () (require 'edit-common)))
(require 'edit-autosave)
(auto-save-enable)
(require 'edit-xah-fly-keys)

                                        ; use ivy for searching
(require 'init-ivy)
(require 'init-origami)
                                        ; code edit

(require 'lang-python)
(require 'lang-c)

                                        ; code compilation

(require 'complete-company)
(add-hook 'prog-mode-hook (lambda () (require 'complete-lsp)))

                                        ; other modes
(require 'init-org)
(add-hook 'org-mode-hook (lambda () (require 'init-roam)))
(require 'reading-nov)
(require 'init-git)
