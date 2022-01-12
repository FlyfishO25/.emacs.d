;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))
;; (require 'awesome-tab)
;; (awesome-tab-mode t)

;; (global-set-key (kbd "s-1") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "s-2") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "s-3") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "s-4") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "s-5") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "s-6") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "s-7") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "s-8") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "s-9") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "s-0") 'awesome-tab-select-visible-tab)

;; (when (not (display-graphic-p))
;;   (setq frame-background-mode 'dark)
;;   (setq awesome-tab-display-icon 't))

 (use-package centaur-tabs
   :config
   (setq centaur-tabs-style "slant"
	  centaur-tabs-height 27
	  centaur-tabs-set-icons t
	  centaur-tabs-set-modified-marker "*"
	  centaur-tabs-set-bar 'under
	  x-underline-at-descent-line t)
   (centaur-tabs-headline-match)
   ;; (setq centaur-tabs-gray-out-icons 'buffer)
   ;; (centaur-tabs-enable-buffer-reordering)
   ;; (setq centaur-tabs-adjust-buffer-order t)
   (centaur-tabs-mode t)
   (setq uniquify-separator "/")
   (setq uniquify-buffer-name-style 'forward)
   (defun centaur-tabs-buffer-groups ()
     "`centaur-tabs-buffer-groups' control buffers' group rules.
 Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
     (list
      (cond
	;; ((not (eq (file-remote-p (buffer-file-name)) nil))
	;; "Remote")
	((or (string-equal "*" (substring (buffer-name) 0 1))
	     (memq major-mode '(magit-process-mode
				magit-status-mode
				magit-diff-mode
				magit-log-mode
				magit-file-mode
				magit-blob-mode
				magit-blame-mode
				)))
	 "Emacs")
	((derived-mode-p 'prog-mode)
	 "Editing")
	((derived-mode-p 'dired-mode)
	 "Dired")
	((memq major-mode '(helpful-mode
			    help-mode))
	 "Help")
	((memq major-mode '(org-mode
			    org-agenda-clockreport-mode
			    org-src-mode
			    org-agenda-mode
			    org-beamer-mode
			    org-indent-mode
			    org-bullets-mode
			    org-cdlatex-mode
			    org-agenda-log-mode
			    diary-mode))
	 "OrgMode")
	(t
	 (centaur-tabs-get-group-name (current-buffer))))))
   :hook
   (dashboard-mode . centaur-tabs-local-mode)
   (term-mode . centaur-tabs-local-mode)
   (calendar-mode . centaur-tabs-local-mode)
   (org-agenda-mode . centaur-tabs-local-mode)
   (helpful-mode . centaur-tabs-local-mode)
   :bind
   ("C-<prior>" . centaur-tabs-backward)
   ("C-<next>" . centaur-tabs-forward)
   ("C-c t s" . centaur-tabs-counsel-switch-group)
   ("C-c t p" . centaur-tabs-group-by-projectile-project)
   ("C-c t g" . centaur-tabs-group-buffer-groups))

