;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python mode settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default python-indent 4)
(setq-default python-indent-offset 4)
(add-hook 'python-mode-hook
          (lambda ()
            (setq tab-width 4)))
(setq-default pdb-command-name "python -m pdb")
(use-package elpy
  :ensure t
  :defer t
  :commands (elpy-enable)
  :after python
  :config
  (elpy-enable)
  )

(use-package yapfify
  :ensure t
  :defer t
  :init
  (add-hook 'python-mode-hook 'yapf-mode))

