;;; -*- lexical-binding: t; -*-
(use-package doom-modeline
      :ensure t
      :defer t
      :hook (after-init . doom-modeline-init)
      :init
      (setq doom-modeline-minor-modes t)
      (setq doom-modeline-height 30)
)

(when (>= emacs-major-version 25.2)
  (use-package minions
    :hook (doom-modeline-mode . minions-mode)))

;; (use-package spaceline)
;; (require 'spaceline-config)
;; (spaceline-spacemacs-theme)
(provide 'ui-doomline)
