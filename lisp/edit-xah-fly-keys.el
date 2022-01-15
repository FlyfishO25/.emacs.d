(use-package xah-fly-keys
  :load-path "site-lisp/"
  :demand
  :init (setq xah-fly-use-meta-key nil
              xah-fly-use-control-key nil)
  :config
  ;; (eval-and-compile (require 'xah-fly-keys))
  (xah-fly-keys-set-layout "qwerty")
  (defun my-config-xah-fly-key-command ()
    "Modify keys for xah fly key command mode keys
to be added to `xah-fly-command-mode-activate-hook'"
    (interactive)
    (define-key xah-fly-key-map (kbd "n") 'swiper)
    (define-key xah-fly-key-map (kbd "2") 'delete-window)
    ;; more here
    )

  (defun my-config-xah-fly-key-insert ()
    "Modify keys for xah fly key command mode keys
to be added to `xah-fly-insert-mode-activate-hook'"
    (interactive)
    (define-key xah-fly-key-map (kbd "M-<SPC>") 'xah-fly-command-mode-activate)
    ;; more here
    )

  (add-hook 'xah-fly-command-mode-activate-hook 'my-config-xah-fly-key-command)
  (add-hook 'xah-fly-insert-mode-activate-hook 'my-config-xah-fly-key-insert)

  (xah-fly-keys 1)
  )
