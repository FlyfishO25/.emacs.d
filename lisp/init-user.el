(unless (file-exists-p (expand-file-name "config-user.el" user-emacs-directory))
  (copy-file (expand-file-name "config-user.example.el" user-emacs-directory)
             (expand-file-name "config-user.el"         user-emacs-directory)))

(load (expand-file-name "config-user.el" user-emacs-directory))

(unless (boundp 'my:compile-command)
  (setq my:compile-command ""))
