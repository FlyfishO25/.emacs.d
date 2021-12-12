;;; -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure t
  :requires dash
  :after (ivy)
  :commands (magit-checkout)
  :bind (("M-g M-s" . magit-status)
         ("M-g M-c" . 'magit-checkout)
         )
  :config
  (add-hook 'magit-mode-hook (lambda () (setq whitespace-mode -1)))
  (setq magit-completing-read-function 'ivy-completing-read)
  )
(use-package magit-gerrit
  :ensure t
  :after magit
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diff-hl (instead of git-gutter)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package git-gutter
;;   :ensure t
;;   :init
;;   (eval-when-compile
;;     ;; Silence missing function warnings
;;     (declare-function global-git-gutter-mode "git-gutter.el"))
;;   :config
;;   ;; If you enable global minor mode
;;   (global-git-gutter-mode t)
;;   ;; Auto update every 5 seconds
;;   (custom-set-variables
;;    '(git-gutter:update-interval 5))

;;   ;; Set the foreground color of modified lines to something obvious
;;   (set-face-foreground 'git-gutter:modified "purple")
;;   )

(use-package diff-hl
  :init
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode)
  ;; Highlight changes on editing.
  (diff-hl-flydiff-mode)
  ;; Makes fringe and margin react to mouse clicks to show the curresponding hunk.
  (diff-hl-show-hunk-mouse-mode)
  :custom
  (diff-hl-draw-borders nil)
  :custom-face
  (diff-hl-change ((t (:background "#8fe9e3"))))
  (diff-hl-insert ((t (:background "#80f1a4"))))
  (diff-hl-delete ((t (:background "#f5cce1")))))

(setq vc-follow-symlinks t)

(provide 'git-package)
