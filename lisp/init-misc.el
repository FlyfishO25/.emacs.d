(setq custom-file (concat user-emacs-directory "/lisp/config-custom.el"))
(load custom-file 'noerror)

(fset 'yes-or-no-p 'y-or-n-p)

