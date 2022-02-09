;;; -*- lexical-binding: t; -*-
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

;; (use-package spaceline)
;; (require 'spaceline-config)
;; (spaceline-spacemacs-theme)

