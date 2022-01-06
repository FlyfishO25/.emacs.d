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
(load-file (concat user-emacs-directory "config-user.el"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(zzz-to-char yasnippet-snippets yapfify window-numbering which-key use-package treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired toc-org solaire-mode rainbow-delimiters paradox page-break-lines ox-gfm origami org-tree-slide org-timeline org-superstar org-rich-yank org-preview-html org-pomodoro org-mime org-fragtog org-fancy-priorities ob-rust ob-mermaid ob-go nov multiple-cursors modern-cpp-font-lock minions major-mode-hydra magit-gerrit lsp-ui lsp-ivy linum-off ivy-rich hlinum good-scroll gnu-elpa-keyring-update flycheck-posframe exec-path-from-shell elpy doom-themes doom-modeline diminish diff-hl dashboard dap-mode counsel-etags company-quickhelp-terminal company-prescient company-box cnfonts clang-format circadian beacon auto-package-update async anzu all-the-icons-ivy all-the-icons-dired aggressive-indent)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
 '(diff-hl-change ((t (:background "#8fe9e3"))))
 '(diff-hl-delete ((t (:background "#f5cce1"))))
 '(diff-hl-insert ((t (:background "#80f1a4"))))
 '(doom-modeline-buffer-file ((t (:inherit (mode-line bold)))))
 '(flycheck-posframe-background-face ((t (:inherit tooltip))))
 '(flycheck-posframe-border-face ((t (:inherit font-lock-comment-face))))
 '(flycheck-posframe-face ((t (:foreground "#7bc275"))))
 '(flycheck-posframe-info-face ((t (:foreground "#7bc275"))))
 '(lsp-headerline-breadcrumb-path-error-face ((t :underline (:style wave :color "#ff665c") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-hint-face ((t :underline (:style wave :color "#7bc275") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-info-face ((t :underline (:style wave :color "#7bc275") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-warning-face ((t :underline (:style wave :color "#FCCE7B") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-symbols-error-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#ff665c"))))
 '(lsp-headerline-breadcrumb-symbols-hint-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#7bc275"))))
 '(lsp-headerline-breadcrumb-symbols-info-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#7bc275"))))
 '(lsp-headerline-breadcrumb-symbols-warning-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#FCCE7B"))))
 '(lsp-ui-sideline-code-action ((t (:inherit warning))))
 '(org-ellipsis ((t (:foreground nil)))))
