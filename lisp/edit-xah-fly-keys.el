(unless (package-installed-p 'xah-fly-keys)
  (package-refresh-contents)
  (package-install 'xah-fly-keys))

(setq xah-fly-use-control-key nil)


(eval-and-compile (require 'xah-fly-keys))

(xah-fly-keys-set-layout "qwerty")

(defun my-config-xah-fly-key ()
  "Modify keys for xah fly key command mode keys
To be added to `xah-fly-command-mode-activate-hook'"
  (interactive)
  (define-key xah-fly-key-map (kbd "n") 'swiper)
  ;; more here
  )

(add-hook 'xah-fly-command-mode-activate-hook 'my-config-xah-fly-key)

(xah-fly-keys 1)

(provide 'edit-xah-fly-keys)
