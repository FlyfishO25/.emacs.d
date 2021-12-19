;; (unless (package-installed-p 'xah-fly-keys)
;;   (package-refresh-contents)
;;   (package-install 'xah-fly-keys))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))

(setq xah-fly-use-control-key nil)
(setq xah-fly-use-meta-key nil)

(eval-and-compile (require 'xah-fly-keys))

(xah-fly-keys-set-layout "qwerty")

(defun my-config-xah-fly-key-command ()
  "Modify keys for xah fly key command mode keys
To be added to `xah-fly-command-mode-activate-hook'"
  (interactive)
  (define-key xah-fly-key-map (kbd "n") 'swiper)
  ;; more here
  )

(defun my-config-xah-fly-key-insert ()
  "Modify keys for xah fly key command mode keys
To be added to `xah-fly-insert-mode-activate-hook'"
  (interactive)
  (define-key xah-fly-key-map (kbd "M-<SPC>") 'xah-fly-command-mode-activate)
  ;; more here
  )


(add-hook 'xah-fly-command-mode-activate-hook 'my-config-xah-fly-key-command)
(add-hook 'xah-fly-insert-mode-activate-hook 'my-config-xah-fly-key-insert)

(xah-fly-keys 1)

(provide 'edit-xah-fly-keys)
