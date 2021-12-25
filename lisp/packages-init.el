;;; -*- lexical-binding: nil; -*-
(require 'package)

(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ;; ("org" . "http://orgmode.org/elpa/")
                         ("gnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ))
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)



;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

;; A modern Packages Menu
(use-package paradox
  :hook (after-init . paradox-enable)
  :init (setq paradox-execute-asynchronously t
              paradox-github-token t
              paradox-display-star-count nil)
  :config
  (when (fboundp 'page-break-lines-mode)
    (add-hook 'paradox-after-execute-functions
              (lambda (&rest _)
                "Display `page-break-lines' in \"*Paradox Report*\"."
                (let ((buf (get-buffer "*Paradox Report*"))
                      (inhibit-read-only t))
                  (when (buffer-live-p buf)
                    (with-current-buffer buf
                      (page-break-lines-mode 1)))))
              t)))

(use-package async
  :ensure t)

(use-package s
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-package-update
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto update packages once a week
(use-package auto-package-update
  :ensure t
  :commands (auto-package-update-maybe)
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe)
  (add-hook 'auto-package-update-before-hook
            (lambda () (message "I will update packages now")))
  )


(provide 'packages-init)
