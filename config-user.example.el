;; Example configure

;; Proxies
;; (setq url-proxy-services '(("http"     . "example.proxy.com:port")
;;                            ("https"    . "example.proxy.com:port")
;;                            ("no_proxy" . "regex.domains.com")))

;; Packages
;; set your package archives here
;; You can use:
;; flymacs-package-archives-bfsu
;; flymacs-package-archives-tsinghua
;; flymacs-package-archives-tencent
;; flymacs-package-archives-origin (default)
;; (setq package-archives flymacs-package-archives-bfsu)

;; Compile
;; (setq my:compile-command "")

;; Keybinding
;; (setq flymacs-keybinding 'xah) ; 'xah, 'evil, or 'emacs

;; Theme
;; (setq flymacs-theme-auto '(("7:00"  . doom-one-light)
;;                            ("17:30" . doom-vibrant)))
;; You can also set it to:
;; (setq flymacs-theme-auto 'doom-one)

;; C/C++
;; (setq flymacs-cpp-style "google") ;; see C-h v c-style-alist or google

;; Entertainment
;; (setq flymacs-games t) ;; t or nil means need games or not

;; Plugins
;; (setq flymacs-latitude {your-latitude}) ;; set your latitude if you want to enable sky-color-clock plugin

;; (use-package vterm) ;; you can use (use-package) macro

;; (use-package-hook! lsp-mode ;; use use-package-hook! macro to hook configured packages (from doom-emacs)
;;   :post-config
;;   (setq lsp-headerline-breadcrumb-enable t))

(provide 'config-user)
;;; config-user.el ends here
