;;; -*- lexical-binding: t; -*-

(use-package all-the-icons)
(use-package all-the-icons-dired)

(setq x-stretch-cursor t)

(defun icons-displayable-p ()
  "Return non-nil if `all-the-icons' is displayable."
  (and display-icon
       (display-graphic-p)
       (require 'all-the-icons nil t)
       (all-the-icons-dired-mode)))

(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))

(defun childframe-workable-p ()
  "Test whether childframe is workable."
       (eq centaur-completion-style 'childframe)
       (not (or noninteractive
                emacs-basic-display
                (not (display-graphic-p)))))

(setq x-stretch-cursor t)

(if (fboundp 'display-line-numbers-mode)
    (use-package display-line-numbers
      :ensure nil
      :hook ((prog-mode yaml-mode) . display-line-numbers-mode))
  (use-package linum-off
    :demand
    :defines linum-format
    :hook (after-init . global-linum-mode)
    :init (setq linum-format "%4d ")
    :config
    ;; Highlight current line number
    (use-package hlinum
      :defines linum-highlight-in-all-buffersp
      :custom-face (linum-highlight-face ((t (:inherit default :background nil :foreground nil))))
      :hook (global-linum-mode . hlinum-activate)
      :init (setq linum-highlight-in-all-buffersp t))))

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t)

;; Display dividers between windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)
;; Set the font to size 9 (90/10).
;(set-face-attribute 'default nil :height my-font-size)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000
      auto-window-vscroll nil
      scroll-preserve-screen-position t)

;; Good pixel line scrolling
(when (and (>= emacs-major-version 27)
           (not (eq system-type 'darwin)))
  (use-package good-scroll
    :diminish
    :hook (after-init . good-scroll-mode)
    :bind (([remap next] . good-scroll-up-full-screen)
           ([remap prior] . good-scroll-down-full-screen))))

(use-package page-break-lines
  :diminish
  :hook (after-init . global-page-break-lines-mode))

(use-package solaire-mode
  :config
  (solaire-global-mode +1)
)

(setq inhibit-splash-screen t)

;; (when (display-graphic-p)
;;   (set-fontset-font t '(#xe903 . #xfffd) "all-the-icons")
;;   (set-fontset-font t '(#x00a2 . #xf17b) "file-icons")
;;   (set-fontset-font t '(#x2665 . #xf27c) "github-octicons")
;;   (set-fontset-font t '(#x2122 . #xf2b4) "FontAwesome")
;;   (set-fontset-font t '(#xf000 . #xf0eb) "Weather Icons"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rainbow Delimiters -  have delimiters be colored by their depth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-delimiters
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function rainbow-delimiters-mode "rainbow-delimiters.el"))
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Beacon-mode: flash the cursor when switching buffers or scrolling
;;              the goal is to make it easy to find the cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package beacon
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function beacon-mode "beacon.el"))
  :config
  (beacon-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key: when you pause on a keyboard shortcut it provides
;;            suggestions in a popup buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package which-key
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function which-key-mode "which-key.el"))
  )

(which-key-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window numbering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package window-numbering installed from package list
;; Allows switching between buffers using meta-(# key)
(use-package window-numbering
  :ensure t
  :config
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function window-numbering-mode "window-numbering.el"))
  (window-numbering-mode t)
  )

(provide 'ui-configure)
