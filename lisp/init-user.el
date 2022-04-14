(if (file-exists-p (expand-file-name "config-user.el" user-emacs-directory))
    (load (expand-file-name "config-user.el" user-emacs-directory))
  )
