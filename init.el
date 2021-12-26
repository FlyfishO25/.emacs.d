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
(require 'init-autoloads)
(require 'init-compile)

(require 'ui-cnfont)
(require 'ui-tree)
(require 'ui-doomline)
;; (require 'ui-tab)
(require 'init-hydra)
(require 'ui-configure)
(require 'ui-dashboard)
(require 'ui-theme)

;; (if (display-graphic-p) (load-theme 'doom-one t) (load-theme 'doom-Iosvkem t))

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
(require 'git-package)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d268b67e0935b9ebc427cad88ded41e875abfcc27abd409726a92e55459e0d01" "6c531d6c3dbc344045af7829a3a20a09929e6c41d7a7278963f7d3215139f6a7" "5784d048e5a985627520beb8a101561b502a191b52fa401139f4dd20acb07607" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "d9646b131c4aa37f01f909fbdd5a9099389518eb68f25277ed19ba99adeb7279" "716f0a8a9370912d9e6659948c2cb139c164b57ef5fda0f337f0f77d47fe9073" "9b54ba84f245a59af31f90bc78ed1240fca2f5a93f667ed54bbf6c6d71f664ac" "333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5" "266ecb1511fa3513ed7992e6cd461756a895dcc5fef2d378f165fed1c894a78c" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "6c98bc9f39e8f8fd6da5b9c74a624cbb3782b4be8abae8fd84cbc43053d7c175" "cbdf8c2e1b2b5c15b34ddb5063f1b21514c7169ff20e081d39cf57ffee89bc1e" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "0d01e1e300fcafa34ba35d5cf0a21b3b23bc4053d388e352ae6a901994597ab1" "246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c" "613aedadd3b9e2554f39afe760708fc3285bf594f6447822dd29f947f0775d6c" "f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" "4b6b6b0a44a40f3586f0f641c25340718c7c626cbf163a78b5a399fbe0226659" "97db542a8a1731ef44b60bc97406c1eb7ed4528b0d7296997cbb53969df852d6" "e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" default))
 '(doom-modeline-mode t)
 '(ns-alternate-modifier '(:ordinary meta :function meta :mouse meta))
 '(ns-command-modifier nil)
 '(package-selected-packages
   '(flycheck aggressive-indent aggressive-indent-mode circadian zzz-to-char lsp-treemacs dap-mode lsp-ivy lsp-ui lsp-mode multiple-cursors anzu yasnippet-snippets window-numbering which-key use-package treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired toc-org rainbow-delimiters page-break-lines ox-gfm origami org-tree-slide org-timeline org-superstar org-rich-yank org-preview-html org-pomodoro org-mime org-fragtog org-fancy-priorities ob-rust ob-mermaid ob-go nov minions markdown-mode major-mode-hydra magit-gerrit linum-off ivy-rich hlinum good-scroll gnu-elpa-keyring-update flycheck-posframe exec-path-from-shell doom-themes doom-modeline diminish diff-hl dashboard counsel-etags company-prescient company-box cnfonts beacon auto-package-update async all-the-icons-ivy)))
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
 '(flycheck-posframe-face ((t (:foreground "ForestGreen"))))
 '(flycheck-posframe-info-face ((t (:foreground "ForestGreen"))))
 '(ivy-posframe ((t (:inherit tooltip))))
 '(ivy-posframe-border ((t (:background "#5B6268"))))
 '(lsp-headerline-breadcrumb-path-error-face ((t :underline (:style wave :color "#ff6c6b") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-hint-face ((t :underline (:style wave :color "#98be65") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-info-face ((t :underline (:style wave :color "#98be65") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-warning-face ((t :underline (:style wave :color "#ECBE7B") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-symbols-error-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#ff6c6b"))))
 '(lsp-headerline-breadcrumb-symbols-hint-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#98be65"))))
 '(lsp-headerline-breadcrumb-symbols-info-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#98be65"))))
 '(lsp-headerline-breadcrumb-symbols-warning-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#ECBE7B"))))
 '(lsp-ui-sideline-code-action ((t (:inherit warning))))
 '(org-ellipsis ((t (:foreground nil))))
 '(org-pomodoro-mode-line ((t (:inherit warning))))
 '(org-pomodoro-mode-line-break ((t (:inherit success))))
 '(org-pomodoro-mode-line-overtime ((t (:inherit error)))))
