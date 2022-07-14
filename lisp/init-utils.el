;;; init-utils.el --- init useful tools  -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable terminal emacs to copy and paste from system clipboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun osx-copy (beg end)
  (interactive "r")
  (call-process-region beg end  "pbcopy"))

(defun osx-paste ()
  (interactive)
  (if (region-active-p) (delete-region (region-beginning) (region-end)) nil)
  (call-process "pbpaste" nil t nil))

(defun flymacs-enable-mail ()
  (interactive)
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/mu4e"))
  (loadpkg 'init-mail)
  )

;; Update
;; function fron https://github.com/seagle0128/.emacs.d/blob/754eb554ca2dd22807898bd5a4257a57f6ab5cfd/lisp/init-funcs.el#L426
(defun update-config ()
  "Update Emacs configurations to the latest version."
  (interactive)
  (let ((emacs-dir (expand-file-name user-emacs-directory)))
    (if (file-exists-p emacs-dir)
        (progn
          (message "Updating configurations...")
          (cd emacs-dir)
          (shell-command "git pull")
          (shell-command "git submodule update --remote")
          (message "Updating configurations...done"))
      (message "\"%s\" doesn't exist" emacs-dir))))

(defalias 'update-packages #'auto-package-update-now)

(defun rename-this-file (new-name)
  ;; from https://github.com/seagle0128/.emacs.d/blob/754eb554ca2dd22807898bd5a4257a57f6ab5cfd/lisp/init-funcs.el#L97
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

