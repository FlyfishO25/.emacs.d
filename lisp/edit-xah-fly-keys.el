(use-package xah-fly-keys)
(xah-fly-keys-set-layout "qwerty")

(xah-fly-keys 1)

(defun my-config-xah-fly-key ()
  "Modify keys for xah fly key command mode keys
To be added to `xah-fly-command-mode-activate-hook'"
  (interactive)
  (define-key xah-fly-key-map (kbd "n") 'swiper)
  ;; more here
  )

(add-hook 'xah-fly-command-mode-activate-hook 'my-config-xah-fly-key)

(provide 'edit-xah-fly-keys)
