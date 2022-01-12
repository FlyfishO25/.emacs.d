(use-package pretty-hydra
  :bind (("C-c h t" . toggles-hydra/body)
	 ("C-c h o" . org-mode-hydra/body)
         ("C-c h m" . multiple-cursors-hydra/body))
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
          (height (or height 1.0))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (icons-displayable-p) icon-type icon-name)
         (let ((f (intern (format "all-the-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face))))

  ;; Global toggles
  (with-no-warnings
    (pretty-hydra-define toggles-hydra (:title (pretty-hydra-title "Toggles" 'faicon "toggle-on" :v-adjust -0.1)
                                               :color amaranth :quit-key "q")
      ("Basic"
       (("n" (if (fboundp 'display-line-numbers-mode)
                 (display-line-numbers-mode (if display-line-numbers-mode -1 1))
               (global-linum-mode (if global-linum-mode -1 1)))
         "line number"
         :toggle (or (bound-and-true-p display-line-numbers-mode) global-linum-mode))
        ("a" global-aggressive-indent-mode "aggressive indent" :toggle t)
        ("d" global-hungry-delete-mode "hungry delete" :toggle t)
        ("e" electric-pair-mode "electric pair" :toggle t)
        ("c" flyspell-mode "spell check" :toggle t)
        ("s" prettify-symbols-mode "pretty symbol" :toggle t)
        ("l" global-page-break-lines-mode "page break lines" :toggle t)
        ("b" display-battery-mode "battery" :toggle t)
        ("i" display-time-mode "time" :toggle t)
        ("m" doom-modeline-mode "modern mode-line" :toggle t))
       "Highlight"
       (("h l" global-hl-line-mode "line" :toggle t)
        ("h p" show-paren-mode "paren" :toggle t)
        ("h s" symbol-overlay-mode "symbol" :toggle t)
        ("h r" rainbow-mode "rainbow" :toggle t)
        ("h w" (setq-default show-trailing-whitespace (not show-trailing-whitespace))
         "whitespace" :toggle show-trailing-whitespace)
        ("h d" rainbow-delimiters-mode "delimiter" :toggle t))
       ;; ("h i" highlight-indent-guides-mode "indent" :toggle t)
       ;; ("h t" global-hl-todo-mode "todo" :toggle t))
       "Program"
       (("f" flycheck-mode "flycheck" :toggle t)
        ("F" flymake-mode "flymake" :toggle t)
        ("o" origami-mode "folding" :toggle t)
        ("O" hs-minor-mode "hideshow" :toggle t)
        ("u" subword-mode "subword" :toggle t)
        ("W" which-function-mode "which function" :toggle t)
        ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
        ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit))
        ("v" global-diff-hl-mode "gutter" :toggle t)
        ("V" diff-hl-flydiff-mode "live gutter" :toggle t)
        ("M" diff-hl-margin-mode "margin gutter" :toggle t)
        ("D" diff-hl-dired-mode "dired gutter" :toggle t))
       ))))

(use-package major-mode-hydra
  :after hydra
  :preface
  (defun with-alltheicon (icon str &optional height v-adjust face)
    "Display an icon from all-the-icon."
    (s-concat (all-the-icons-alltheicon icon :v-adjust (or v-adjust 0) :height (or height 1) :face face) " " str))

  (defun with-faicon (icon str &optional height v-adjust face)
    "Display an icon from Font Awesome icon."
    (s-concat (all-the-icons-faicon icon ':v-adjust (or v-adjust 0) :height (or height 1) :face face) " " str))

  (defun with-fileicon (icon str &optional height v-adjust face)
    "Display an icon from the Atom File Icons package."
    (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1) :face face) " " str))

  (defun with-octicon (icon str &optional height v-adjust face)
    "Display an icon from the GitHub Octicons."
    (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1) :face face) " " str)))

(pretty-hydra-define org-mode-hydra
  (:title (pretty-hydra-title "Org Mode" 'fileicon "org" :face 'all-the-icons-green :height 1.1 :v-adjust 0.0)
           :color blue :quit-key "q")
  ("Task"
   (("n" org-meta-return "new")
    ("m" org-time-stamp "insert date" :exit t)
    )
   "Time"
   (("s" org-clock-in "start timer" :color blue :exit t)
    ("t" org-clock-out "stop timer" :color blue :exit t)
    ("S" org-schedule "set SCHEDULED" :color purple)
    ("D" org-deadline "set DEADLINE" :color purple :exit t)
    )
   "Actions"
   (("x" org-ctrl-c-ctrl-c "toggle checkbox" :color red)
    ("b" org-todo "toggle status" :color red)
    ("v" org-toggle-inline-images "view images")
    ("l" org-metaleft "move left" :color blue)
    ("r" org-metaright "move right" :color blue))
   "Move"
    (("w" previous-line "↑")
     ("s" next-line "↓")
     ("a" backward-char "←")
     ("d" forward-char "→")
    )
  ))

(pretty-hydra-define multiple-cursors-hydra
  (:title (pretty-hydra-title "Multiple Cursor" 'faicon "i-cursor" :face 'all-the-icons-green :height 1.1 :v-adjust 0.0)
          :color blue :quit-key "q")
  ("Mark"
   ((">" mc/mark-next-like-this "mark next like this" :exit t)
    ("<" mc/mark-previous-like-this "mark previous like this" :exit t)
    ("a" mc/mark-all-like-this "mark all like this" :exit t))
   "Sort"
   (("s" mc/sort-regions "sort the marked regions alphabetically" :exit t))
   "Insert"
   (("i" mc/insert-numbers "insert increasing numbers" :exit t)
    ("I" mc/insert-letters "insert increasing letters" :exit t))
   ))
