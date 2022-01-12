 (use-package dashboard
    :diminish dashboard-mode
    :functions (all-the-icons-faicon
                all-the-icons-material)
    :custom-face (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
    :init
    (setq dashboard-banner-logo-title "GNU EMACS"
          dashboard-startup-banner 'logo
          dashboard-page-separator "\n\f\n"
          dashboard-center-content t
          dashboard-show-shortcuts nil
          dashboard-items '((recents  . 10)
                            (bookmarks . 5)
                            )

          dashboard-set-init-info t
          dashboard-heading-icons '((recents   . "file-text")
                                    (bookmarks . "bookmark")
                                    (agenda    . "calendar")))

     (dashboard-setup-startup-hook))
