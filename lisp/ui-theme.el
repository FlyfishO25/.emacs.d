;;; -*- lexical-binding: t; -*-
(use-package doom-themes
  :ensure t
  :defer
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

(use-package circadian
  :ensure t
  :hook ('after-make-frame-functions . (lambda ()
                                         (circadian-setup)))
  :config
  ;; use Guangzhou, China as default
  (setq calendar-latitude 23.130280
        calendar-longitude 113.288879)
  
  (setq circadian-themes '((:sunrise . doom-one-light)
                           (:sunset . doom-vibrant)))
  )

(circadian-setup)

(provide 'ui-theme)
