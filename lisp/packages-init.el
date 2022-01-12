;;; -*- lexical-binding: nil; -*-
(setq flymacs-package-archives-tsinghua '(("mepla" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                                             ("elpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
      
      flymacs-package-archives-bfsu '(("elpa" . "https://mirrors.bfsu.edu.cn/elpa/gnu/")
                                         ("melpa" . "https://mirrors.bfsu.edu.cn/elpa/melpa/"))

      flymacs-package-archives-tencent '(("elpa" . "https://mirrors.cloud.tencent.com/elpa/gnu/")
                                            ("melpa" . "https://mirrors.cloud.tencent.com/elpa/melpa/"))

      flymacs-package-archives-origin '(("melpa" . "https://melpa.org/packages/")
                                        ("elpa" . "https://elpa.gnu.org/packages/")))

(setq package-archives flymacs-package-archives-bfsu)

(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

(require 'package)

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

;; @see https://github.com/jwiegley/use-package/issues/383
(defun flymacs-package--save-selected-packages (&optional value)
  "Set and (don't!) save `package-selected-packages' to VALUE."
  (when value
    (setq package-selected-packages value))
  (unless after-init-time
    (add-hook 'after-init-hook #'package--save-selected-packages)))

(advice-add 'package--save-selected-packages :override #'flymacs-package--save-selected-packages)

(use-package async
  :ensure t)

(use-package s
  :ensure t)

(use-package system-packages)

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
