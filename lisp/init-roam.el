(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/roam/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(setq org-roam-v2-ack t)

(unless (package-installed-p 'org-roam-ui)
  (package-refresh-contents)
  (package-install 'org-roam-ui))

;; (add-to-list 'load-path "~/.emacs.d/private/org-roam-ui")
;; (load-library "org-roam-ui")

