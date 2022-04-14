;; Example configure
;; Warning: DO NOT use use-package here! Put them in config-user.el instead!

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
;; Or use straight.el
;; (setq flymacs-use-straight t)

;; Compile
;; (setq my:compile-command "")

;; Keybinding
;; (setq flymacs-keybinding 'xah) ; 'xah, 'evil, or 'emacs

;; Theme
;; (setq flymacs-theme-auto '(("7:00"  . doom-one-light)
;;                            ("17:30" . doom-vibrant)))
;; You can also set it to:
;; (setq flymacs-theme-auto 'doom-one)

;; UI
;; (setq flymacs-ui 'rich) ; 'rich or 'simple to choose different ui type
;; If you like a more colorful, rich UI with icons, choose 'rich.
;; Otherwise, choose 'simple will give you a clean, simple UI.

;; C/C++
;; (setq flymacs-cpp-style "google") ;; see C-h v c-style-alist or google

;; Entertainment
;; (setq flymacs-games t) ;; t or nil means need games or not

;; Plugins
;; (setq flymacs-latitude 23) ;; set your latitude if you want to enable sky-color-clock plugin
;; (setq flymacs-sky-clock-api-key "API-key")"cd1ff2c003a45c5acf4bb27803e4aabb"
;; (setq flymacs-sky-color-city city-code)

;; UI
;; (setq display-icon t) ;; non-nil if you want to display icons (from centaur emacs)
;; (setq completion-style 'childframe) ;; 'childframe or 'minibuffer to set different type of flycheck errors (from centaur emacs)

;; LSP
;; (setq centaur-lsp 'lsp-mode) ;; 'lsp-mode or 'eglot to set lsp client (from centaur emacs)

;; Server
;; (setq flymacs-server t) ;; non-nil to enable server-mode at startup

(provide 'config-user-option)
;;; config-user-option.el ends here
