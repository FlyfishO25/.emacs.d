;; Don't load outdated lisp files
(setq load-prefer-newer t)

;; Speed up startup
(setq auto-mode-case-fold nil)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(setq read-process-output-max (* 1024 1024)) ;; 1mb
(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 100000000
                  gc-cons-percentage 0.1)))

(provide 'init-performance)
