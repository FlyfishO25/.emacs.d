(defun flymacs-install-fonts (&optional arg)
  "Helper function to download and install Fira code and all the icons font.
When arg is non-nil, install font without prompt."
  (interactive "P")
  (when (or arg (yes-or-no-p "Download and install fonts?"))
    (all-the-icons-install-fonts t)
    (let* ((url-format "https://raw.githubusercontent.com/FlyfishO25/.emacs.d/fonts/FiraCode/%s")
           (font-dest (cond
                       ;; Default Linux install directories
                       ((member system-type '(gnu gnu/linux gnu/kfreebsd))
                        (concat (or (getenv "XDG_DATA_HOME")
                                    (concat (getenv "HOME") "/.local/share"))
                                "/fonts/"))
                       ;; Default MacOS install directory
                       ((eq system-type 'darwin)
                        (concat (getenv "HOME") "/Library/Fonts/"))))
           (known-dest? (stringp font-dest))
           (font-dest (or font-dest (read-directory-name "Font installation directory: " "~/"))))

      (unless (file-directory-p font-dest) (mkdir font-dest t))

      (mapc (lambda (font)
              (url-copy-file (format url-format font) (expand-file-name font font-dest) t))
            all-the-icons-font-names)
      (when known-dest?
        (message "Fonts downloaded, updating font cache... <fc-cache -f -v> ")
        (shell-command-to-string (format "fc-cache -f -v")))
      (message "%s Successfully %s `all-the-icons' fonts to `%s'!"
               (all-the-icons-wicon "stars" :v-adjust 0.0)
               (if known-dest? "installed" "downloaded")
               font-dest)))
  )

(provide 'install)
