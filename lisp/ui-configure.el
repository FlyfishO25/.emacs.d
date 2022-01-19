;;; -*- lexical-binding: t; -*-
(require 'init-funcs)

(setq frame-title-format
      '(:eval (concat
	       (if (and buffer-file-name (buffer-modified-p)) "•")
	       (buffer-name)
	       (if buffer-file-name
		   (concat " (" (directory-file-name (abbreviate-file-name default-directory)) ")"))
	       " - Emacs"))
      )

(use-package all-the-icons
  :config
  (with-no-warnings
    (defun all-the-icons-reset ()
      "Reset the icons."
      (interactive)
      (dolist (func '(all-the-icons-icon-for-dir
                      all-the-icons-icon-for-file
                      all-the-icons-icon-for-mode
                      all-the-icons-icon-for-url
                      all-the-icons-icon-family-for-file
                      all-the-icons-icon-family-for-mode
                      all-the-icons-icon-family))
        (all-the-icons-cache func))
      (message "Reset all-the-icons")))

  ;; Support more icons
  (let ((extension-icon-alist
         '(("bat"  all-the-icons-alltheicon "terminal" :face all-the-icons-lsilver)
           ("cmd"  all-the-icons-alltheicon "terminal" :face all-the-icons-lsilver)
           ("conf" all-the-icons-octicon "settings"    :v-adjust 0.0 :face all-the-icons-yellow)
           ("eln"  all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-dsilver)
           ("epub" all-the-icons-faicon "book"         :height 1.0 :v-adjust -0.1 :face all-the-icons-green)
           ("exe"  all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-dsilver)
           ("make" all-the-icons-fileicon "gnu"        :face all-the-icons-dorange)
           ("rss"  all-the-icons-octicon "rss"         :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange)
           ("toml" all-the-icons-octicon "settings"    :v-adjust 0.0 :face all-the-icons-yellow)
           ("tsx"  all-the-icons-fileicon "tsx"        :height 1.0 :v-adjust -0.1 :face all-the-icons-cyan-alt)
           ("xpm"  all-the-icons-octicon "file-media"  :v-adjust 0.0 :face all-the-icons-dgreen))))
    (dolist (icon extension-icon-alist)
      (add-to-list 'all-the-icons-extension-icon-alist icon)))

  (let ((regexp-icon-alist
         '(("\\.[bB][iI][nN]$"               all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-yellow)
           ("^config$"                       all-the-icons-octicon "settings"    :v-adjust 0.0 :face all-the-icons-dorange)
           ("\\.\\(ba\\|z\\)shrc$"           all-the-icons-alltheicon "script"   :height 0.9 :face all-the-icons-dpink)
           ("\\.\\(bash\\|zsh\\)*_?profile$" all-the-icons-alltheicon "script"   :height 0.9 :face all-the-icons-dred)
           ("\\.\\(ba\\|z\\)sh_history$"     all-the-icons-alltheicon "script"   :height 0.9 :face all-the-icons-dsilver)
           ("\\.zshenv$"                     all-the-icons-alltheicon "script"   :height 0.9 :face all-the-icons-dred)
           ("Cask\\'"                        all-the-icons-fileicon "elisp"      :height 1.0 :v-adjust -0.2 :face all-the-icons-blue)
           ("NEWS$"                          all-the-icons-faicon "newspaper-o"  :height 0.9 :v-adjust -0.2)
           ("^Rakefile$"                     all-the-icons-alltheicon "ruby-alt" :face all-the-icons-red)
           ("^go.\\(sum\\|mod\\)$"           all-the-icons-fileicon "go"         :face all-the-icons-dpurple))))
    (dolist (icon regexp-icon-alist)
      (add-to-list 'all-the-icons-regexp-icon-alist icon)))

  (let ((mode-icon-alist
         '((xwidget-webkit-mode           all-the-icons-faicon "chrome"          :v-adjust -0.1 :face all-the-icons-blue)
           (bongo-playlist-mode           all-the-icons-material "queue_music"   :height 1.2 :face all-the-icons-green)
           (bongo-library-mode            all-the-icons-material "library_music" :height 1.1 :face all-the-icons-green)
           (gnus-group-mode               all-the-icons-fileicon "gnu"           :face all-the-icons-silver)
           (gnus-summary-mode             all-the-icons-octicon "inbox"          :height 1.0 :v-adjust 0.0 :face all-the-icons-orange)
           (gnus-article-mode             all-the-icons-octicon "mail"           :height 1.1 :v-adjust 0.0 :face all-the-icons-lblue)
           (message-mode                  all-the-icons-octicon "mail"           :height 1.1 :v-adjust 0.0 :face all-the-icons-lblue)
           (diff-mode                     all-the-icons-octicon "git-compare"    :v-adjust 0.0 :face all-the-icons-lred)
           (flycheck-error-list-mode      all-the-icons-octicon "checklist"      :height 1.1 :v-adjust 0.0 :face all-the-icons-lred)
           (elfeed-search-mode            all-the-icons-faicon "rss-square"      :v-adjust -0.1 :face all-the-icons-orange)
           (elfeed-show-mode              all-the-icons-octicon "rss"            :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange)
           (newsticker-mode               all-the-icons-faicon "rss-square"      :v-adjust -0.1 :face all-the-icons-orange)
           (newsticker-treeview-mode      all-the-icons-faicon "rss-square"      :v-adjust -0.1 :face all-the-icons-orange)
           (newsticker-treeview-list-mode all-the-icons-octicon "rss"            :height 1.1 :v-adjust 0.0 :face all-the-icons-orange)
           (newsticker-treeview-item-mode all-the-icons-octicon "rss"            :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange)
           (conf-mode                     all-the-icons-octicon "settings"       :v-adjust 0.0 :face all-the-icons-yellow)
           (conf-space-mode               all-the-icons-octicon "settings"       :v-adjust 0.0 :face all-the-icons-yellow)
           (gitconfig-mode                all-the-icons-octicon "settings"       :v-adjust 0.0 :face all-the-icons-dorange)
           (forge-topic-mode              all-the-icons-alltheicon "git"         :face all-the-icons-blue)
           (help-mode                     all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1 :face all-the-icons-purple)
           (helpful-mode                  all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1 :face all-the-icons-purple)
           (Info-mode                     all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1)
           (cask-mode                     all-the-icons-fileicon "elisp"         :height 1.0 :v-adjust -0.2 :face all-the-icons-blue)
           (ein:notebooklist-mode         all-the-icons-faicon "book"            :face all-the-icons-lorange)
           (ein:notebook-mode             all-the-icons-fileicon "jupyter"       :height 1.2 :face all-the-icons-orange)
           (ein:notebook-multilang-mode   all-the-icons-fileicon "jupyter"       :height 1.2 :face all-the-icons-dorange)
           (nov-mode                      all-the-icons-faicon "book"            :height 1.0 :v-adjust -0.1 :face all-the-icons-green)
           (gfm-mode                      all-the-icons-octicon "markdown"       :face all-the-icons-lblue)
           (osx-dictionary-mode           all-the-icons-material "library_books" :face all-the-icons-lblue)
           (youdao-dictionary-mode        all-the-icons-material "library_books" :face all-the-icons-lblue)
           (fanyi-mode                    all-the-icons-material "library_books" :face all-the-icons-lblue))))
    (dolist (icon mode-icon-alist)
      (add-to-list 'all-the-icons-mode-icon-alist icon))))

(setq x-stretch-cursor t)

(use-package all-the-icons-dired
  :diminish
  :if (icons-displayable-p)
  :hook (dired-mode . all-the-icons-dired-mode)
  :init (setq all-the-icons-dired-monochrome nil)
  )

(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))

(defun childframe-workable-p ()
  "Test whether childframe is workable."
  (eq completion-style 'childframe)
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
  :defer 0.8
  :diminish
  :config
  (solaire-global-mode +1)
  )

(setq inhibit-splash-screen t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rainbow Delimiters -  have delimiters be colored by their depth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-delimiters
  :defer 3
  :diminish
  :hook (prog-mode-hook . rainbow-delimiters-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Beacon-mode: flash the cursor when switching buffers or scrolling
;;              the goal is to make it easy to find the cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package beacon
  :config
  :diminish
  (beacon-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key: when you pause on a keyboard shortcut it provides
;;            suggestions in a popup buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package which-key
  :defer 1
  :diminish
  :custom (which-key-idle-delay 0.5)
  )

(which-key-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window numbering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package window-numbering installed from package list
;; Allows switching between buffers using meta-(# key)
(use-package window-numbering
  :defer 1.3
  :config
  (window-numbering-mode 1))
