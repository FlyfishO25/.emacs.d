(if (not (file-exists-p (concat user-emacs-directory "config-user.el")))
    (copy-file (concat user-emacs-directory "config-user.example.el") (concat user-emacs-directory "config-user.el")))

(load (concat user-emacs-directory "config-user.el"))
