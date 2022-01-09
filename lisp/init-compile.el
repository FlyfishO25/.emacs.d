(use-package exec-path-from-shell
  :ensure t
  :config
  (if (and (fboundp 'native-comp-available-p)
                (native-comp-available-p))
      (progn
        (message "Native comp is available")
        ;; Using Emacs.app/Contents/MacOS/bin since it was compiled with
        ;; ./configure --prefix="$PWD/nextstep/Emacs.app/Contents/MacOS"
        ;; Append to path to give priority to values from exec-path-from-shell-initialize.
        (add-to-list 'exec-path (concat invocation-directory (file-name-as-directory "bin")) t)
        (setenv "LIBRARY_PATH" (concat (getenv "LIBRARY_PATH")
                                       (when (getenv "LIBRARY_PATH")
                                         ":")
                                       ;; This is where Homebrew puts libgccjit libraries.
                                       (car (file-expand-wildcards
                                             ;; (expand-file-name "/opt/homebrew/opt/libgccjit/lib/gcc/*")
                                             (expand-file-name "/usr/local/Cellar/libgccjit/11.2.0_1/lib/gcc/11/")
                                             ))))
        ;; Only set after LIBRARY_PATH can find gcc libraries.
        (defvar comp-deferred-compilation t)
        (setq package-native-compile t)
        (defvar comp-speed 3))
    (message "Native comp is *not* available")))

(setq exec-path-from-shell-arguments (list "-l"))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically compile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun byte-compile-init-files (file)
  "Automatically compile FILE."
  (interactive)
  (save-restriction
    ;; Suppress the warning when you setq an undefined variable.
    (if (>= emacs-major-version 23)
        (setq byte-compile-warnings '(not free-vars obsolete cl-function))
      (setq byte-compile-warnings
            '(unresolved
              callargs
              redefine
              obsolete
              noruntime
              cl-warnings
              interactive-only)))
    (byte-compile-file (expand-file-name file)))
  )

(add-hook
 'after-save-hook
 (function
  (lambda ()
    (if (string= (file-truename "~/.emacs.d/init.el")
                 (file-truename (buffer-file-name)))
        (byte-compile-init-files (file-truename "~/.emacs.d/init.el")))
    )
  )
 )

;; Byte-compile again to ~/.emacs.elc if it is outdated
(if (file-newer-than-file-p
     (file-truename "~/.emacs.d/init.el")
     (file-truename "~/.emacs.d/init.elc"))
    (byte-compile-init-files "~/.emacs.d/init.el"))

(if (file-newer-than-file-p
     (file-truename "~/.emacs.d/config-user.el")
     (file-truename "~/.emacs.d/config-user.elc"))
    (byte-compile-init-files "~/.emacs.d/config-user.el"))
