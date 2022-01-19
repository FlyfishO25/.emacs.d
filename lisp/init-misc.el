(use-package no-littering               ; Keep .emacs.d clean
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(load custom-file 'noerror)



(fset 'yes-or-no-p 'y-or-n-p)

