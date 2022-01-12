(defun flymacs-git-merge-p ()
  "Return non-nil if using flymacs to edit git merge message."
  (boundp 'startup-now))

(defun loadpkg (name &optional must-be-loaded)
  "Load customization files"
  (when (or (not (flymacs-git-merge-p)) must-be-loaded)
    (load (file-truename (concat user-emacs-directory (format "lisp/%s" name))) t t)))

(defun flymacs-post-install ()
  (if (not (eq system-type "windows-nt"))
    (when (not (executable-find "rg")
               (system-packages-install "ripgrep"))
      (message "Cannot find ripgrep executable (rg), please install it.")))
)

(provide 'init-funcs)
