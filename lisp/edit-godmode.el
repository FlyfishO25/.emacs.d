(setq god-mode-enable-function-key-translation nil)
(use-package god-mode)
(god-mode)
;; (global-set-key (kbd "<escape>") #'god-local-mode)
(global-set-key (kbd "<escape>") #'god-mode-all)
(provide 'edit-godmode)
