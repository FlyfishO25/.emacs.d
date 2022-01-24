;;; init-misc.el --- initialization miscellaneous -*- lex
(use-package no-littering               ; Keep .emacs.d clean
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(load custom-file 'noerror)

(defun launch-separate-emacs-in-terminal ()
  "Launch another Emacs process in terminal."
  (suspend-emacs "fg ; emacs -nw"))

(defun launch-separate-emacs-under-gui ()
  "Launch another Emacs process under GUI."
  (call-process "sh" nil nil nil "-c" "emacs &"))

(defun restart-emacs ()
  "Restart Emacs from within Emacs."
  (interactive)
  (let ((kill-emacs-hook (append kill-emacs-hook (list (if (display-graphic-p)
                                                           #'launch-separate-emacs-under-gui
                                                         #'launch-separate-emacs-in-terminal)))))
    (save-buffers-kill-emacs)))

(fset 'yes-or-no-p 'y-or-n-p)

;;; init-misc.el ends here
