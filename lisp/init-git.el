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
  (setq magit-completing-read-function 'ivy-completing-read)
  )

(use-package magit-gerrit
  :ensure t
  :after magit
  )

(use-package git-timemachine
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit success))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
  :bind (:map vc-prefix-map
         ("t" . git-timemachine))
  :hook ((git-timemachine-mode . (lambda ()
                                   "Display different colors in mode-line."
                                   (face-remap-add-relative 'mode-line 'custom-saved)))
         (before-revert . (lambda ()
                            (when (bound-and-true-p git-timemachine-mode)
                              (user-error "Cannot revert the timemachine buffer"))))))



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

;; Highlight uncommitted changes using VC
;; @see https://github.com/seagle0128/.emacs.d/blob/e840ab62fd5f1a8df9818d0678e7413145e4c8d3/lisp/init-highlight.el#L223
(use-package diff-hl
  :custom-face
  (diff-hl-change ((t (:foreground ,(face-background 'highlight) :background nil))))
  (diff-hl-insert ((t (:inherit diff-added :background nil))))
  (diff-hl-delete ((t (:inherit diff-removed :background nil))))
  :bind (:map diff-hl-command-map
         ("SPC" . diff-hl-mark-hunk))
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :init (setq diff-hl-draw-borders nil)
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)

  ;; Set fringe style
  (setq-default fringes-outside-margins t)

  ;; Reset faces after changing the color theme
  (add-hook 'after-load-theme-hook
            (lambda ()
              (custom-set-faces
               `(diff-hl-change ((t (:foreground ,(face-background 'highlight) :background nil))))
               '(diff-hl-insert ((t (:inherit diff-added :background nil))))
               '(diff-hl-delete ((t (:inherit diff-removed :background nil)))))))

  (with-no-warnings
    (defun my-diff-hl-fringe-bmp-function (_type _pos)
      "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
      (define-fringe-bitmap 'my-diff-hl-bmp
        (vector (if (eq system-type 'darwin) #b11100000 #b11111100))
        1 8
        '(center t)))
    (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)

    (when (or (not (display-graphic-p)) (daemonp))
      ;; Fall back to the display margin since the fringe is unavailable in tty
      (diff-hl-margin-mode 1)
      ;; Avoid restoring `diff-hl-margin-mode'
      (with-eval-after-load 'desktop
        (add-to-list 'desktop-minor-mode-table
                     '(diff-hl-margin-mode nil))))

    ;; Integration with magit
    (with-eval-after-load 'magit
      (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))))

(setq vc-follow-symlinks t)
