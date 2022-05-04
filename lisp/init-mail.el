
;;; Code:

(use-package mu4e
  :defer t
  :load-path "site-lisp/mu4e/"
  :commands (mu4e))
(setq mail-user-agent 'mu4e-user-agent)
(setq mu4e-maildir "~/.mail")
(setq mu4e-get-mail-command "offlineimap -u quiet")
(setq mu4e-update-interval 60)

(setq mu4e-view-show-images t)
