;; Example configure

;; Proxies
;; (setq url-proxy-services '(("http" . "127.0.0.1:7890")))
;; (setq url-proxy-services '(("https" . "127.0.0.1:7890")))

;; Compile
(setq my:compile-command "~/tools/cpp11")

;; Keybinding
(setq flymacs-keybinding 'xah) ;; 'xah, 'evil, or 'emacs

;; Themes
(setq flymacs-theme-auto '(("7:00" . doom-nord)
                           ("17:30" . doom-dracula)))
;; (setq flymacs-theme-auto 'modus-operandi)
(setq flymacs-ui 'rich)

(setq flymacs-cpp-style "google")

(setq flymacs-games t)

(setq package-archives flymacs-package-archives-bfsu)

(setq flymacs-latitude 23)

(setq flymacs-server t)

(setq flymacs-use-straight t)
 ;; (setq flymacs-theme-auto 'doom-one)
