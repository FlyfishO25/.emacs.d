;; initfile --- Summary:
;;; Commentary:
;; Emacs 25.1 and newer tested
;; Keys:
;; C-c C-w copy->clipboard
;; C-c C-y paste<-clipboard
;; C-c h help
;; C-x u undo
;; M-TAB word complete
;; C-c ; comment & uncomment
;; C-c C-c compile
;; C-s search
;; C-c o origami(a for all)
;; M-c / M-s -> Goto char/word
;; C-e -> end of line
;; C-a -> start of line
;; M-< -> start of buffer
;; M-> -> end of buffer
;; C-h delete a char backward
;; M-h delete a word backward

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration/Customization:
;; Defines global variables that are later used to customize and set
;; up packages.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; global configure

(setq package-archives '(("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set packages to install
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable package initialize after us.  We either initialize it
;; anyway in case of interpreted .emacs, or we don't want slow
;; initizlization in case of byte-compiled .emacs.elc.
(setq package-enable-at-startup nil)
;; Ask package.el to not add (package-initialize) to .emacs.
(setq package--init-file-ensured t)
;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))
;; Add the macro generated list of package.el loadpaths to load-path.
(mapc #'(lambda (add) (add-to-list 'load-path add))
      (eval-when-compile
        (require 'package)
        (package-initialize)
        ;; Install use-package if not installed yet.
        (unless (package-installed-p 'use-package)
          (package-refresh-contents)
          (package-install 'use-package))
        ;; (require 'use-package)
        (let ((package-user-dir-real (file-truename package-user-dir)))
          ;; The reverse is necessary, because outside we mapc
          ;; add-to-list element-by-element, which reverses.
          (nreverse (apply #'nconc
                           ;; Only keep package.el provided loadpaths.
                           (mapcar #'(lambda (path)
                                       (if (string-prefix-p package-user-dir-real path)
                                           (list path)
                                         nil))
                                   load-path))))))
                                   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; By default Emacs triggers garbage collection at ~0.8MB which makes
;; startup really slow. Since most systems have at least 64MB of memory,
;; we increase it during initialization.
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

;; proxy configure

;(setq url-proxy-services '(("http" . "127.0.0.1:8118")))
;(setq url-proxy-services '(("https" . "127.0.0.1:8118")))

;; theme configure

;; (load-theme 'monokai t)
;; (use-package melancholy-theme
;;   :ensure t)

(use-package all-the-icons)

(setq display-icon t)

(defun icons-displayable-p ()
  "Return non-nil if `all-the-icons' is displayable."
  (and display-icon
       (display-graphic-p)
       (require 'all-the-icons nil t)))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-material t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; ycmd configure (code complete)

;; Specify the ycmd server command and path to the ycmd directory *inside* the
;; cloned ycmd directory
(defvar my:ycmd-server-command '("python3" "/Users/markzhou/.emacs.d/ycmd/ycmd"))
(defvar my:ycmd-extra-conf-whitelist '("/Users/markzhou/.ycm_extra_conf.py"))
(defvar my:ycmd-global-config "/Users/markzhou/.ycm_extra_conf.py")

(setq ycmd-startup-timeout 10)

;; Specify the jupyter executable name, and the start dir of the server
(defvar my:jupyter_location (executable-find "jupyter"))
(defvar my:jupyter_start_dir "/Users/markzhou/Documents")

;; compile configure

;; Compilation command for C/C++
(defvar my:compile-command "~/tools/cpp11 ")



;; Extra plugins and config files are stored here
(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start emacs server if not already running
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; turn on highlight matching brackets when cursor is on one
(show-paren-mode t)
;; move window smooth
(windmove-default-keybindings)
;; Overwrite region selected
(delete-selection-mode t)
;; Show column numbers by default
(setq column-number-mode t)
;; Use CUA to delete selections
(setq cua-mode t)
(setq cua-enable-cua-keys nil)
;; Prevent emacs from creating a bckup file filename~
(setq make-backup-files nil)
;; Settings for searching
(setq-default case-fold-search t ;case insensitive searches by default
              search-highlight t) ;hilit matches when searching
;; Highlight the line we are currently on
(global-hl-line-mode -1)
;; Disable the toolbar at the top since it's useless
;; (if (functionp 'tool-bar-mode) (tool-bar-mode -1))
(tool-bar-mode 0)
(menu-bar-mode 0)

;; Remove trailing white space upon saving
;; Note: because of a bug in EIN we only delete trailing whitespace
;; when not in EIN mode.
(add-hook 'before-save-hook
          (lambda ()
            (when (not (derived-mode-p 'ein:notebook-multilang-mode))
              (delete-trailing-whitespace))))

;; Auto-wrap at 80 characters
(setq-default auto-fill-function 'do-auto-fill)
(setq-default fill-column 80)
(turn-on-auto-fill)
;; Disable auto-fill-mode in programming mode
(add-hook 'prog-mode-hook (lambda () (auto-fill-mode -1)))

;; Global Keyboard Shortcuts
;; Set help to C-?
;; (global-set-key (kbd "C-c h") 'help-command)
(global-set-key (kbd "C-c h") help-map)
;; Set mark paragraph to M-?
(global-set-key (kbd "M-?") 'mark-paragraph)
;; Set backspace to C-h
(global-set-key (kbd "C-h") 'delete-backward-char)
;; Set backspace word to M-h
(global-set-key (kbd "M-h") 'backward-kill-word)
;; Use meta+tab word completion
(global-set-key (kbd "M-TAB") 'dabbrev-expand)
;; Easy undo key
(global-set-key (kbd "C-/") 'undo)
;; Comment or uncomment the region
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
;; Indent after a newline, if required by syntax of language
(global-set-key (kbd "C-m") 'newline-and-indent)
;; Load the compile ocmmand
(global-set-key (kbd "C-c C-c") 'compile)
;; Undo, basically C-x u
;; (global-set-key (kbd "C-/") 'undo)
;; Find file in project
(global-set-key (kbd "C-x M-f") 'project-find-file)
;; complete the code
(global-set-key (kbd "C-c TAB") 'company-complete)
;; ;; Add some regular used keys
;; (setq mac-option-modifier 'meta
;;         mac-command-modifier 'super)
;;   (global-set-key [(super a)] #'mark-whole-buffer)
;;   ;; (global-set-key [(super v)] #'yank)
;;   ;; (global-set-key [(super c)] #'kill-ring-save)
;;   (global-set-key [(super s)] #'save-buffer)
;;   (global-set-key [(super l)] #'goto-line)
;;   (global-set-key [(super w)] #'delete-frame)
;;   (global-set-key [(super z)] #'undo)))
;; ;; save file
;; (global-set-key (kbd "s-s") 'save-buffer)
;; ;; kill buffer
;; (global-set-key (kbd "s-w") 'kill-buffer)
;; ;; undo
;; (global-set-key (kbd "s-z") 'undo)

;; We don't want to type yes and no all the time so, do y and n
(defalias 'yes-or-no-p 'y-or-n-p)
;; Disable the horrid auto-save
(setq auto-save-default nil)

;; Disable the menu bar since we don't use it, especially not in the
;; terminal
(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))

;; Don't ring the bell
(setq ring-bell-function 'ignore)

;; Non-nil means draw block cursor as wide as the glyph under it.
;; For example, if a block cursor is over a tab, it will be drawn as
;; wide as that tab on the display.
(setq x-stretch-cursor t)

;; Dont ask to follow symlink in git
(setq vc-follow-symlinks t)

;; Check (on save) whether the file edited contains a shebang, if yes,
;; make it executable from
;; http://mbork.pl/2015-01-10_A_few_random_Emacs_tips
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; Highlight some keywords in prog-mode
(add-hook 'prog-mode-hook
          (lambda ()
            ;; Highlighting in cmake-mode this way interferes with
            ;; cmake-font-lock, which is something I don't yet understand.
            (when (not (derived-mode-p 'cmake-mode))
              (font-lock-add-keywords
               nil
               '(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\)"
                  1 font-lock-warning-face t))))
            )
          )

;; Setup use-package
(eval-when-compile
  (require 'use-package))
(use-package bind-key
  :ensure t)
;; so we can (require 'use-package) even in compiled emacs to e.g. read docs
(use-package use-package
  :commands use-package-autoload-keymap)

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

;; (defun linux-copy (beg end)
;;   (interactive "r")
;;   (call-process-region beg end  "xclip" nil nil nil "-selection" "c"))

;; (defun linux-paste ()
;;   (interactive)ctive-p) (delete-region (region-beginning) (region-end)) nil)
;;   (call-process "xsel" nil t nil "-b"))

(global-set-key (kbd "C-c M-w") 'osx-copy)
;;(global-set-key (kbd "C-c M-w") 'my-copy-to-xclipboard)
(global-set-key (kbd "C-c C-y") 'osx-paste)
;; (global-set-key (kbd "s-a") 'mark-whole-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; async - library for async/thread processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package async
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; s is used by ycmd, origami, etc and sometimes during Emacs
;; upgrades disappears so we try to install it on its own.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package s
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically compile and save ~/.emacs.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun byte-compile-init-files (file)
  "Automatically compile FILE."
  (interactive)
  (save-restriction
    ;; Suppress the warning when you setq an undefined variable.
    (if (>= emacs-major-version 23)
        (setq byte-compile-warnings '(not free-vars obsolete))
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
    (if (string= (file-truename "~/.emacs.el")
                 (file-truename (buffer-file-name)))
        (byte-compile-init-files (file-truename "~/.emacs.el")))
    )
  )
 )

;; Byte-compile again to ~/.emacs.elc if it is outdated
(if (file-newer-than-file-p
     (file-truename "~/.emacs.el")
     (file-truename "~/.emacs.elc"))
    (byte-compile-init-files "~/.emacs.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-package-update
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto update packages once a week
(use-package auto-package-update
  :ensure t
  :commands (auto-package-update-maybe)
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe)
  (add-hook 'auto-package-update-before-hook
          (lambda () (message "I will update packages now")))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package ivy
  :ensure t
  :commands (ivy-mode)
  :config
  (require 'ivy)
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-wrap t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  ;; Show #/total when scrolling buffers
  (setq ivy-count-format "%d/%d ")
  )

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ("C-r" . swiper))
  )

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git-grep)
         ("C-c j" . counsel-git)
         ("C-c k" . counsel-ag)
         ("C-c r" . counsel-rg)
         ("C-x l" . counsel-locate)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-add)
         )
  :config
  (if (executable-find "rg")
      ;; use ripgrep instead of grep because it's way faster
      (setq counsel-grep-base-command
            "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
            counsel-rg-base-command
            "rg -i -M 120 --no-heading --line-number --color never %s ."
            )
    (warn "\nWARNING: Could not find the ripgrep executable. It "
          "is recommended you install ripgrep.")
    )
  )

;; Use universal ctags to build the tags database for the project.
;; When you first want to build a TAGS database run 'touch TAGS'
;; in the root directory of your project.
(use-package counsel-etags
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function counsel-etags-virtual-update-tags "counsel-etags.el")
    (declare-function counsel-etags-guess-program "counsel-etags.el")
    (declare-function counsel-etags-locate-tags-file "counsel-etags.el"))
  :bind (
         ("M-." . counsel-etags-find-tag-at-point)
         ("M-t" . counsel-etags-grep-symbol-at-point)
         ("M-s" . counsel-etags-find-tag))
  :config
  ;; Ignore files above 800kb
  (setq counsel-etags-max-file-size 800)
  ;; Ignore build directories for tagging
  (add-to-list 'counsel-etags-ignore-directories '"build*")
  (add-to-list 'counsel-etags-ignore-directories '".vscode")
  (add-to-list 'counsel-etags-ignore-filenames '".clang-format")
  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)
  ;; How many seconds to wait before rerunning tags for auto-update
  (setq counsel-etags-update-interval 180)
  ;; Set up auto-update
  (add-hook
   'prog-mode-hook
   (lambda () (add-hook 'after-save-hook
                        (lambda ()
                          (counsel-etags-virtual-update-tags))))
   )

  ;; The function provided by counsel-etags is broken (at least on Linux)
  ;; and doesn't correctly exclude directories, leading to an excessive
  ;; amount of incorrect tags. The issue seems to be that the trailing '/'
  ;; in e.g. '*dirname/*' causes 'find' to not correctly exclude all files
  ;; in that directory, only files in sub-directories of the dir set to be
  ;; ignore.
  (defun my-scan-dir (src-dir &optional force)
    "Create tags file from SRC-DIR. \
     If FORCE is t, the commmand is executed without \
     checking the timer."
    (let* ((find-pg (or
                     counsel-etags-find-program
                     (counsel-etags-guess-program "find")))
           (ctags-pg (or
                      counsel-etags-tags-program
                      (format "%s -e -L" (counsel-etags-guess-program
                                          "ctags"))))
           (default-directory src-dir)
           ;; run find&ctags to create TAGS
           (cmd (format
                 "%s . \\( %s \\) -prune -o -type f -not -size +%sk %s | %s -"
                 find-pg
                 (mapconcat
                  (lambda (p)
                    (format "-iwholename \"*%s*\"" p))
                  counsel-etags-ignore-directories " -or ")
                 counsel-etags-max-file-size
                 (mapconcat (lambda (n)
                              (format "-not -name \"%s\"" n))
                            counsel-etags-ignore-filenames " ")
                 ctags-pg))
           (tags-file (concat (file-name-as-directory src-dir) "TAGS"))
           (doit (or force (not (file-exists-p tags-file)))))
      ;; always update cli options
      (when doit
        (message "%s at %s" cmd default-directory)
        (shell-command cmd)
        (visit-tags-table tags-file t)
        )
      )
    )

  (setq counsel-etags-update-tags-backend
        (lambda ()
          (interactive)
          (let* ((tags-file (counsel-etags-locate-tags-file)))
            (when tags-file
              ('my-scan-dir (file-name-directory tags-file) t)
              (run-hook-with-args
               'counsel-etags-after-update-tags-hook tags-file)
              (unless counsel-etags-quiet-when-updating-tags
                (message "%s is updated!" tags-file))))
          )
        )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window numbering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package window-numbering installed from package list
;; Allows switching between buffers using meta-(# key)
(use-package window-numbering
  :ensure t
  :config
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function window-numbering-mode "window-numbering.el"))
  (window-numbering-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wgrep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wgrep allows you to edit all files in a grep result. For example,
;; you can use C-c g or C-c r to search all files in a project, then
;; use C-c C-o to enter ivy-occur mode, followed by 'w' to make
;; the grep results buffer editable, then you can edit the results
;; however you wish.
(use-package wgrep
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edit server to allow editing of things in Chrome with Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package edit-server
;;   :ensure t
;;   :config
;;   (progn
;;     (eval-when-compile
;;       ;; Silence missing function warnings
;;       (declare-function edit-server-start "edit-server-start.el"))
;;     (when (daemonp)
;;       (edit-server-start)
;;       )
;;     (add-hook 'edit-server-start-hook
;;               (lambda ()
;;                 (when (string-match "github.com" (buffer-name))
;;                   (markdown-mode))))
;;     )
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Origami - Does code folding, ie hide the body of an
;; if/else/for/function so that you can fit more code on your screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package origami
  :ensure t
  :commands (origami-mode)
  :bind (:map origami-mode-map
              ("C-c o :" . origami-recursively-toggle-node)
              ("C-c o a" . origami-toggle-all-nodes)
              ("C-c o t" . origami-toggle-node)
              ("C-c o o" . origami-show-only-node)
              ("C-c o u" . origami-undo)
              ("C-c o U" . origami-redo)
              ("C-c o C-r" . origami-reset)
              )
  :config
  (setq origami-show-fold-header t)
  ;; The python parser currently doesn't fold if/for/etc. blocks, which is
  ;; something we want. However, the basic indentation parser does support
  ;; this with one caveat: you must toggle the node when your cursor is on
  ;; the line of the if/for/etc. statement you want to collapse. You cannot
  ;; fold the statement by toggling in the body of the if/for/etc.
  (add-to-list 'origami-parser-alist '(python-mode . origami-indent-parser))
  :init
  (add-hook 'prog-mode-hook 'origami-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rainbow Delimiters -  have delimiters be colored by their depth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-delimiters
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function rainbow-delimiters-mode "rainbow-delimiters.el"))
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Beacon-mode: flash the cursor when switching buffers or scrolling
;;              the goal is to make it easy to find the cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package beacon
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function beacon-mode "beacon.el"))
  :config
  (beacon-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key: when you pause on a keyboard shortcut it provides
;;            suggestions in a popup buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package which-key
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function which-key-mode "which-key.el"))
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; avy: always fast jump to char inside the current view buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package avy
  :ensure t
  :bind (("M-c" . avy-goto-char)
         ("M-s" . avy-goto-word-1))
  ;; Set keys for Dvorak mode instead of qwerty
  :init (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s
                            ?A ?O ?E ?U ?I ?D ?H ?T ?N ?S
                            ?p ?y ?f ?g ?c ?r ?l
                            ?P ?Y ?F ?G ?C ?R ?L)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zzz-to-char: replaces the built-in zap-to-char with avy-like
;;              replacement options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package zzz-to-char
  :ensure t
  :bind ("M-z" . zzz-up-to-char))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RealGud - https://github.com/realgud/realgud
;; A rewrite of GUD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(use-package realgud
;  :ensure t
;  :init
;  (setenv "TERM" "dumb")
;  :config
;  (setq realgud:pdb-command-name "python -m pdb"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python mode settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default python-indent 4)
(setq-default python-indent-offset 4)
(add-hook 'python-mode-hook
          (lambda ()
            (setq tab-width 4)))
(setq-default pdb-command-name "python -m pdb")
(use-package elpy
  :ensure t
  :commands (elpy-enable)
  :after python
  :config
  (elpy-enable)
  )

(use-package yapfify
  :ensure t
  :init
  (add-hook 'python-mode-hook 'yapf-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clang-format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clang-format can be triggered using C-c C-f
;; Create clang-format file using google style
;; clang-format -style=google -dump-config > .clang-format
(use-package clang-format
  :ensure t
  :bind (("C-c C-f" . clang-format-region))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modern C++ code highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package modern-cpp-font-lock
  :ensure t
  :init
  (eval-when-compile
      ;; Silence missing function warnings
    (declare-function modern-c++-font-lock-global-mode
                      "modern-cpp-font-lock.el"))
  :config
  (modern-c++-font-lock-global-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cc-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
  :config
  (define-key c++-mode-map (kbd "C-c C-c") 'compile)
  (define-key c++-mode-map (kbd "C-c C-k") 'kill-compilation)
  (setq compile-command my:compile-command)
;;   (use-package google-c-style
;;     :ensure t
;;     :config
;;     ;; This prevents the extra two spaces in a namespace that Emacs
;;     ;; otherwise wants to put... Gawd!
;;     ;; (add-hook 'c-mode-  ;;   (add-hook 'c-mode-common-hook 'google-set-c-style)
;;     ;; Autoindent using google style guide
;; common-hook 'google-make-newline-indent)
  ;;   )
  )

;; Change tab key behavior to insert spaces instead
(setq-default indent-tabs-mode nil)

;; Set the number of spaces that the tab key inserts (usually 2 or 4)
(setq c-basic-offset 4)
;; Set the size that a tab CHARACTER is interpreted as
;; (unnecessary if there are no tab characters in the file!)
(setq tab-width 4)

;; We want to be able to see if there is a tab character vs a space.
;; global-whitespace-mode allows us to do just that.
;; Set whitespace mode to only show tabs, not newlines/spaces.
(use-package whitespace
  :ensure t
  :init
  (eval-when-compile
      ;; Silence missing function warnings
      (declare-function global-whitespace-mode "whitespace.el"))
  :config
  (setq whitespace-style '(tabs tab-mark))
  ;; Turn on whitespace mode globally.
  (global-whitespace-mode t)
  )

;; Enable hide/show of code blocks
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package: ycmd (YouCompleteMeDaemon)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up YouCompleteMe for emacs:
;; https://github.com/Valloric/ycmd
;; https://github.com/abingham/emacs-ycmd
(defvar my:python-location (executable-find (nth 0 my:ycmd-server-command)))
(if (not my:python-location)
    (message
     "Could not start YouCompleteMeDaemon because the python executable could
not be found.\nSpecified executable is: '%s'\nPlease set my:ycmd-server-command
appropriately in ~/.emacs.el.\n" (nth 0 my:ycmd-server-command)))
(if (not (file-directory-p (nth 1 my:ycmd-server-command)))
    (message "Could not YouCompleteMeDaemon because the specified directory does
not exist.\nSpecified directory is: '%s'
Please set my:ycmd-server-command appropriately in ~/.emacs.el.\n"
             (nth 1 my:ycmd-server-command)))
(if (and my:python-location
         (file-directory-p (nth 1 my:ycmd-server-command)))
    (use-package ycmd
      :ensure t
      :init
      (eval-when-compile
        ;; Silence missing function warnings
        (declare-function global-ycmd-mode "ycmd.el"))
      (add-hook 'after-init-hook #'global-ycmd-mode)
      :config
      (progn
        (set-variable 'ycmd-server-command my:ycmd-server-command)
        (set-variable 'ycmd-extra-conf-whitelist my:ycmd-extra-conf-whitelist)
        (set-variable 'ycmd-global-config my:ycmd-global-config)
        (setq ycmd-force-semantic-completion t)
        (use-package company-ycmd
          :ensure t
          :init
          (eval-when-compile
            ;; Silence missing function warnings
            (declare-function company-ycmd-setup "company-ycmd.el"))
          :config
            (setq company-tooltip-align-annotations t ; 注释贴右侧对齐
                  company-tooltip-limit 10            ; 菜单里可选项数量
                  company-show-numbers t              ; 显示编号（然后可以用 M-数字 快速选定某一项）
                  company-idle-delay .1               ; 延时多少秒后弹出
                  company-minimum-prefix-length 1     ; 至少几个字符后开始补全
            )
          (company-ycmd-setup)
          )

        (use-package flycheck-ycmd
          :ensure t
          :init
          (add-hook 'c-mode-common-hook 'flycheck-ycmd-setup)
          )

        ;; Add displaying the function arguments in mini buffer using El Doc
        (require 'ycmd-eldoc)
        (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)
        )
      )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up code completion with company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :ensure t
  :config
  ;; Zero delay when pressing tab
  (setq company-idle-delay 0)
  (add-hook 'after-init-hook 'global-company-mode)
  ;; remove unused backends
  ;; (setq company-backends (delete 'company-semantic company-backends))
  (setq company-backends (delete 'company-eclim company-backends))
  (setq company-backends (delete 'company-xcode company-backends))
  ;(setq company-backends (delete 'company-clang company-backends))
  (setq company-backends (delete 'company-bbdb company-backends))
  (setq company-backends (delete 'company-oddmuse company-backends))
  )

;; Setup loading company-jedi for python completion
;; This requines running jedi:install-server the first time
(use-package company-jedi
  :ensure t
  :after python
  :init
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook)
  )

(use-package company-prescient
    :init (company-prescient-mode 1))

;; Icons and quickhelp
  (if (display-graphic-p)
          (use-package company-box
            :diminish
            :defines company-box-icons-all-the-icons
            :hook (company-mode . company-box-mode)
            :init (setq company-box-enable-icon display-icon
                        company-box-backends-colors nil
                        company-box-doc-delay 0.1)
            :config
            (with-no-warnings
              ;; Prettify icons
              (defun my-company-box-icons--elisp (candidate)
                (when (or (derived-mode-p 'emacs-lisp-mode) (derived-mode-p 'lisp-mode))
                  (let ((sym (intern candidate)))
                    (cond ((fboundp sym) 'Function)
                          ((featurep sym) 'Module)
                          ((facep sym) 'Color)
                          ((boundp sym) 'Variable)
                          ((symbolp sym) 'Text)
                          (t . nil)))))
              (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

              ;; Display borders and optimize performance
              (defun my-company-box--display (string on-update)
                "Display the completions."
                (company-box--render-buffer string on-update)

                (let ((frame (company-box--get-frame))
                      (border-color (face-foreground 'font-lock-comment-face nil t)))
                  (unless frame
                    (setq frame (company-box--make-frame))
                    (company-box--set-frame frame))
                  (company-box--compute-frame-position frame)
                  (company-box--move-selection t)
                  (company-box--update-frame-position frame)
                  (unless (frame-visible-p frame)
                    (make-frame-visible frame))
                  (company-box--update-scrollbar frame t)
                  (set-face-background 'internal-border border-color frame)
                  (when (facep 'child-frame-border)
                    (set-face-background 'child-frame-border border-color frame)))
                (with-current-buffer (company-box--get-buffer)
                  (company-box--maybe-move-number (or company-box--last-start 1))))
              (advice-add #'company-box--display :override #'my-company-box--display)

              (setq company-box-doc-frame-parameters '((internal-border-width . 1)
                                                       (left-fringe . 8)
                                                       (right-fringe . 8)))

              (defun my-company-box-doc--make-buffer (object)
                (let* ((buffer-list-update-hook nil)
                       (inhibit-modification-hooks t)
                       (string (cond ((stringp object) object)
                                     ((bufferp object) (with-current-buffer object (buffer-string))))))
                  (when (and string (> (length (string-trim string)) 0))
                    (with-current-buffer (company-box--get-buffer "doc")
                      (erase-buffer)
                      (insert (propertize "\n" 'face '(:height 0.5)))
                      (insert string)
                      (insert (propertize "\n\n" 'face '(:height 0.5)))

                      ;; Handle hr lines of markdown
                      ;; @see `lsp-ui-doc--handle-hr-lines'
                      (with-current-buffer (company-box--get-buffer "doc")
                        (let (bolp next before after)
                          (goto-char 1)
                          (while (setq next (next-single-property-change (or next 1) 'markdown-hr))
                            (when (get-text-property next 'markdown-hr)
                              (goto-char next)
                              (setq bolp (bolp)
                                    before (char-before))
                              (delete-region (point) (save-excursion (forward-visible-line 1) (point)))
                              (setq after (char-after (1+ (point))))
                              (insert
                               (concat
                                (and bolp (not (equal before ?\n)) (propertize "\n" 'face '(:height 0.5)))
                                (propertize "\n" 'face '(:height 0.5))
                                (propertize " "
                                            'display '(space :height (1))
                                            'company-box-doc--replace-hr t
                                            'face `(:background ,(face-foreground 'font-lock-comment-face)))
                                (propertize " " 'display '(space :height (1)))
                                (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.5)))))))))

                      (setq mode-line-format nil
                            display-line-numbers nil
                            header-line-format nil
                            show-trailing-whitespace nil
                            cursor-in-non-selected-windows nil)
                      (current-buffer)))))
              (advice-add #'company-box-doc--make-buffer :override #'my-company-box-doc--make-buffer)

              ;; Display the border and fix the markdown header properties
              (defun my-company-box-doc--show (selection frame)
                (cl-letf (((symbol-function 'completing-read) #'company-box-completing-read)
                          (window-configuration-change-hook nil)
                          (inhibit-redisplay t)
                          (display-buffer-alist nil)
                          (buffer-list-update-hook nil))
                  (-when-let* ((valid-state (and (eq (selected-frame) frame)
                                                 company-box--bottom
                                                 company-selection
                                                 (company-box--get-frame)
                                                 (frame-visible-p (company-box--get-frame))))
                               (candidate (nth selection company-candidates))
                               (doc (or (company-call-backend 'quickhelp-string candidate)
                                        (company-box-doc--fetch-doc-buffer candidate)))
                               (doc (company-box-doc--make-buffer doc)))
                    (let ((frame (frame-local-getq company-box-doc-frame))
                          (border-color (face-foreground 'font-lock-comment-face nil t)))
                      (unless (frame-live-p frame)
                        (setq frame (company-box-doc--make-frame doc))
                        (frame-local-setq company-box-doc-frame frame))
                      (set-face-background 'internal-border border-color frame)
                      (when (facep 'child-frame-border)
                        (set-face-background 'child-frame-border border-color frame))
                      (company-box-doc--set-frame-position frame)

                      ;; Fix hr props. @see `lsp-ui-doc--fix-hr-props'
                      (with-current-buffer (company-box--get-buffer "doc")
                        (let (next)
                          (while (setq next (next-single-property-change (or next 1) 'company-box-doc--replace-hr))
                            (when (get-text-property next 'company-box-doc--replace-hr)
                              (put-text-property next (1+ next) 'display
                                                 '(space :align-to (- right-fringe 1) :height (1)))
                              (put-text-property (1+ next) (+ next 2) 'display
                                                 '(space :align-to right-fringe :height (1)))))))

                      (unless (frame-visible-p frame)
                        (make-frame-visible frame))))))
              (advice-add #'company-box-doc--show :override #'my-company-box-doc--show)

              (defun my-company-box-doc--set-frame-position (frame)
                (-let* ((frame-resize-pixelwise t)

                        (box-frame (company-box--get-frame))
                        (box-position (frame-position box-frame))
                        (box-width (frame-pixel-width box-frame))
                        (box-height (frame-pixel-height box-frame))
                        (box-border-width (frame-border-width box-frame))

                        (window (frame-root-window frame))
                        ((text-width . text-height) (window-text-pixel-size window nil nil
                                                                            (/ (frame-pixel-width) 2)
                                                                            (/ (frame-pixel-height) 2)))
                        (border-width (or (alist-get 'internal-border-width company-box-doc-frame-parameters) 0))

                        (x (- (+ (car box-position) box-width) border-width))
                        (space-right (- (frame-pixel-width) x))
                        (space-left (car box-position))
                        (fringe-left (or (alist-get 'left-fringe company-box-doc-frame-parameters) 0))
                        (fringe-right (or (alist-get 'right-fringe company-box-doc-frame-parameters) 0))
                        (width (+ text-width border-width fringe-left fringe-right))
                        (x (if (> width space-right)
                               (if (> space-left width)
                                   (- space-left width)
                                 space-left)
                             x))
                        (y (cdr box-position))
                        (bottom (+ company-box--bottom (frame-border-width)))
                        (height (+ text-height (* 2 border-width)))
                        (y (cond ((= x space-left)
                                  (if (> (+ y box-height height) bottom)
                                      (+ (- y height) border-width)
                                    (- (+ y box-height) border-width)))
                                 ((> (+ y height) bottom)
                                  (- (+ y box-height) height))
                                 (t y))))
                  (set-frame-position frame (max x 0) (max y 0))
                  (set-frame-size frame text-width text-height t)))
              (advice-add #'company-box-doc--set-frame-position :override #'my-company-box-doc--set-frame-position)

              (when (icons-displayable-p)
                (setq company-box-icons-all-the-icons
                      `((Unknown . ,(all-the-icons-material "find_in_page" :height 1.0 :v-adjust -0.2))
                        (Text . ,(all-the-icons-faicon "text-width" :height 1.0 :v-adjust -0.02))
                        (Method . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
                        (Function . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
                        (Constructor . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
                        (Field . ,(all-the-icons-octicon "tag" :height 1.1 :v-adjust 0 :face 'all-the-icons-lblue))
                        (Variable . ,(all-the-icons-octicon "tag" :height 1.1 :v-adjust 0 :face 'all-the-icons-lblue))
                        (Class . ,(all-the-icons-material "settings_input_component" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
                        (Interface . ,(all-the-icons-material "share" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
                        (Module . ,(all-the-icons-material "view_module" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
                        (Property . ,(all-the-icons-faicon "wrench" :height 1.0 :v-adjust -0.02))
                        (Unit . ,(all-the-icons-material "settings_system_daydream" :height 1.0 :v-adjust -0.2))
                        (Value . ,(all-the-icons-material "format_align_right" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
                        (Enum . ,(all-the-icons-material "storage" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
                        (Keyword . ,(all-the-icons-material "filter_center_focus" :height 1.0 :v-adjust -0.2))
                        (Snippet . ,(all-the-icons-material "format_align_center" :height 1.0 :v-adjust -0.2))
                        (Color . ,(all-the-icons-material "palette" :height 1.0 :v-adjust -0.2))
                        (File . ,(all-the-icons-faicon "file-o" :height 1.0 :v-adjust -0.02))
                        (Reference . ,(all-the-icons-material "collections_bookmark" :height 1.0 :v-adjust -0.2))
                        (Folder . ,(all-the-icons-faicon "folder-open" :height 1.0 :v-adjust -0.02))
                        (EnumMember . ,(all-the-icons-material "format_align_right" :height 1.0 :v-adjust -0.2))
                        (Constant . ,(all-the-icons-faicon "square-o" :height 1.0 :v-adjust -0.1))
                        (Struct . ,(all-the-icons-material "settings_input_component" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
                        (Event . ,(all-the-icons-octicon "zap" :height 1.0 :v-adjust 0 :face 'all-the-icons-orange))
                        (Operator . ,(all-the-icons-material "control_point" :height 1.0 :v-adjust -0.2))
                        (TypeParameter . ,(all-the-icons-faicon "arrows" :height 1.0 :v-adjust -0.02))
                        (Template . ,(all-the-icons-material "format_align_left" :height 1.0 :v-adjust -0.2)))
                      company-box-icons-alist 'company-box-icons-all-the-icons))))

        ;; Popup documentation for completion candidates
        (use-package company-quickhelp
          :defines company-quickhelp-delay
          :bind (:map company-active-map
                 ([remap company-show-doc-buffer] . company-quickhelp-manual-begin))
          :hook (global-company-mode . company-quickhelp-mode)
          :init (setq company-quickhelp-delay 0.3)))

    ;; Display documentation for completion candidates in terminal
    (use-package company-quickhelp-terminal
      :defines company-quickhelp-delay
      :bind (:map company-active-map
             ([remap company-show-doc-buffer] . company-quickhelp-manual-begin))
      :hook ((global-company-mode . company-quickhelp-mode)
             (company-quickhelp-mode  . company-quickhelp-terminal-mode))
      :init (setq company-quickhelp-delay 0.3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note: For C++ we use flycheck-ycmd
(use-package flycheck
  :ensure t
  :defer t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function global-flycheck-mode "flycheck.el"))
  :config
  ;; Turn flycheck on everywhere
  (global-flycheck-mode t)
  ;; There are issues with company mode and flycheck in terminal mode.
  ;; This is outlined at:
  ;; https://github.com/abingham/emacs-ycmd
  (when (not (display-graphic-p))
    (setq flycheck-indication-mode nil))
  )
(use-package flycheck-pyflakes
  :ensure t
  :after python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string-inflection
;; used for switching between different cases, eg CamelCase,
;; lowerCamelCase, snake_case, and SCREAMING_SNAKE_CASE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package string-inflection
  :ensure t
  :defer t
  :bind (("C-c c i" . string-inflection-cycle)
         ("C-c c l" . string-inflection-lower-camelcase)
         ("C-c c c" . string-inflection-camelcase)
         ("C-c c s" . string-inflection-underscore)
         ("C-c c u" . string-inflection-upcase)
         )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple-cursors  - https://github.com/magnars/multiple-cursors.el
;; Allows you to have multiple cursors on different lines so you can
;; easily edit multiple lines at once.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package multiple-cursors
  :ensure t
  :bind (("M-n" . mc/mark-next-like-this)
         ("M-p" . mc/mark-previous-like-this)
         ("C-c a" . mc/mark-all-like-this)
         ("C-c e" . mc/edit-lines))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-log-done 'time
      org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE"))
      org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))))
;; (use-package writegood-mode
;;   :ensure t
;;   :init
;;   (eval-when-compile
;;     ;; Silence missing function warnings
;;     (declare-function writegood-mode "writegood-mode.el"))
;;   (add-hook 'org-mode-hook #'writegood-mode)
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vlf - handle open very large files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vlf
  :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package web-mode
  :ensure t
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ein - ipython notebooks in gui emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Only launch if the executable exists.
(if (and my:jupyter_location
         my:jupyter_start_dir)
    (use-package ein
      :ensure t
      :commands (ein:jupyter-server-start)
      :defer 5
      :config
      (require 'ein)
      (require 'ein-loaddefs)
      (require 'ein-notebook)
      (require 'ein-subpackages)
      ;; when editing the emacs.el file, we do not want to start a new
      ;; Jupyter server each time we save, so we only start a new Jupyter
      ;; server if there currently isn't one running.
      (defvar my-found-ein-server nil)
      (dolist (my-current-process (process-list))
        (when (string-match "EIN: Jupyter*" (process-name my-current-process))
          (setq my-found-ein-server t))
        )
      (when (not my-found-ein-server)
        (ein:jupyter-server-start my:jupyter_location my:jupyter_start_dir))
      )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autopair
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically at closing brace, bracket and quote
;; (use-package autopair
;;   :ensure t
;;   :init
;;   (eval-when-compile
;;     ;; Silence missing function warnings
;;     (declare-function autopair-global-mode "autopair.el"))
;;   :config
;;   (autopair-global-mode t)
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load hungry Delete, caus we're lazy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set hungry delete:
(use-package hungry-delete
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function global-hungry-delete-mode "hungry-delete.el"))
  :config
  (global-hungry-delete-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax Highlighting in CUDA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load CUDA mode so we get syntax highlighting in .cu files
(use-package cuda-mode
  :ensure t
  :mode (("\\.cu\\'" . cuda-mode)
         ("\\.cuh\\'" . cuda-mode)))

;; I hate flyspee
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Flyspell Mode for Spelling Corrections
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package flyspell
;;   :ensure t
;;   :init
;;   (eval-when-compile
;;     ;; Silence missing function warnings
;;     (declare-function flyspell-goto-next-error "flyspell.el")
;;     (declare-function flyspell-mode "flyspell.el")
;;     (declare-function flyspell-prog-mode "flyspell.el"))
;;   (setq flyspell-issue-welcome-flag nil)
;;   :config
;;   (defun flyspell-check-next-highlighted-word ()
;;     "Custom function to spell check next highlighted word."
;;     (interactive)
;;     (flyspell-goto-next-error)
;;     (ispell-word))

;;   (global-set-key (kbd "<f7>") 'flyspell-buffer)
;;   (global-set-key (kbd "<f8>") 'flyspell-correct-previous)
;;   (global-set-key (kbd "<f9>") 'flyspell-correct-previous)

;;   (add-hook 'text-mode-hook #'flyspell-mode)
;;   (add-hook 'prog-mode-hook #'flyspell-prog-mode)
;;   (add-hook 'org-mode-hook #'flyspell-mode)
;;   )
;; (use-package flyspell-correct-ivy
;;   :ensure t
;;   :after flyspell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure t
  :requires dash
  :after (ivy)
  :commands (magit-checkout)
  :bind (("M-g M-s" . magit-status)
         ("M-g M-c" . 'magit-checkout)
         )
  :config
  (add-hook 'magit-mode-hook (lambda () (setq whitespace-mode -1)))
  (setq magit-completing-read-function 'ivy-completing-read)
  )
(use-package magit-gerrit
  :ensure t
  :after magit
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GitGutter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package git-gutter
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function global-git-gutter-mode "git-gutter.el"))
  :config
  ;; If you enable global minor mode
  (global-git-gutter-mode t)
  ;; Auto update every 5 seconds
  (custom-set-variables
   '(git-gutter:update-interval 5))

  ;; Set the foreground color of modified lines to something obvious
  (set-face-foreground 'git-gutter:modified "purple")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cmake-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists.txt" ".cmake")
  :hook (cmake-mode . (lambda ()
                        (add-to-list 'company-backends 'company-cmake)))
  :config
  (use-package cmake-font-lock
    :ensure t
    :defer t
    :commands (cmake-font-lock-activate)
    :hook (cmake-mode . (lambda ()
                          (cmake-font-lock-activate)
                          (font-lock-add-keywords
                           nil '(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\)"
                                  1 font-lock-warning-face t)))
                          ))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bazel-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (not (file-exists-p "~/.emacs.d/plugins/bazel-mode.el"))
    (url-copy-file
     "https://raw.githubusercontent.com/codesuki/bazel-mode/master/bazel-mode.el"
     "~/.emacs.d/plugins/bazel-mode.el"))
(if (file-exists-p "~/.emacs.d/plugins/bazel-mode.el")
    (use-package bazel-mode
      :mode ("BUILD" "\\.bazel\\'" "\\.bzl'" "WORKSPACE\\'")
      )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; protobuf-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (not (file-exists-p "~/.emacs.d/plugins/protobuf-mode.el"))
    (url-copy-file
     "https://raw.githubusercontent.com/google/protobuf/master/editors/protobuf-mode.el"
     "~/.emacs.d/plugins/protobuf-mode.el"))
(if (file-exists-p "~/.emacs.d/plugins/protobuf-mode.el")
    (use-package protobuf-mode
      :mode ("\\.proto")
      )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yaml-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yaml-mode
  :ensure t
  :mode (".yml" ".yaml"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; json-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package json-mode
  :ensure t
  :mode (".json" ".imp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup Dockerfile mode
;; 1. Download file from GitHub
;; 2. Load mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (not (file-directory-p "~/.emacs.d/plugins"))
    (make-directory "~/.emacs.d/plugins"))
(if (not (file-exists-p "~/.emacs.d/plugins/dockerfile-mode.el"))
    (url-copy-file
     "https://raw.githubusercontent.com/spotify/dockerfile-mode/master/dockerfile-mode.el"
     "~/.emacs.d/plugins/dockerfile-mode.el"))
(use-package dockerfile-mode
  :mode ("Dockerfile"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package: yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yasnippet
  :ensure t
  :commands (yas-reload-all)
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function yas-global-mode "yasnippet.el")
    )
  :defer 5
  :config
  (yas-global-mode t)
  (yas-reload-all))
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yas-reload-all))
;; Apparently the company-yasnippet backend shadows all backends that
;; come after it. To work around this we assign yasnippet to a different
;; keybind since actual source completion is vital.
(use-package company-yasnippet
  :bind ("C-M-y" . company-yasnippet)
  :after (yasnippet)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load asm-mode when opening assembly files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package asm-mode
  :mode ("\\.s\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use markdown-mode for markdown files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :ensure t
  :mode (".md" ".markdown"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auctex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  ;; When we byte-compile we need to have the autoloads loaded in order to
  ;; properly get auctex working, otherwise auctex is not loaded correctly
  :init
  (load "auctex-autoloads" nil t)
  :config
  (setq-default TeX-auto-save t
                TeX-parse-self t
                TeX-source-correlate-start-server t)
  (cond
   ((string-equal system-type "windows-nt") ; Microsoft Windows
    (progn
      (message "Windows does not have a PDF viewer set for auctex")))
   ((string-equal system-type "darwin") ; Mac OS X
    (setq-default
     TeX-view-program-list
     '(("Skim"
        "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")
       )
     TeX-view-program-selection '((output-pdf "Skim"))))
   ((string-equal system-type "gnu/linux") ; linux
    (setq-default TeX-view-program-list
                  '(("Evince" "evince --page-index=%(outpage) %o"))
                  TeX-view-program-selection '((output-pdf "Evince")))))
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-buffer)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq-default reftex-plug-into-AUCTeX t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The deeper blue theme is loaded but the resulting text
;; appears black in Aquamacs. This can be fixed by setting
;; the font color under Menu Bar->Options->Appearance->Font For...
;; and then setting "Adopt Face and Frame Parameter as Frame Default"
;; (use-package sourcerer-theme
;;   :ensure t
;;   :config
;;   (load-theme 'sourcerer t))

;; (set-face-background 'hl-line "#372E2D")
;; ;; The minibuffer default colors with my theme are impossible to read, so change
;; ;; them to something better using ivy-minibuffer-match-face.
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((((type tty) (background dark)) (:background "nil"))))
;;  '(company-preview ((t (:background "#073642" :foreground "#2aa198"))))
;;  '(company-preview-common ((t (:foreground "#93a1a1" :underline t))))
;;  '(company-scrollbar-bg ((t (:background "#073642" :foreground "#2aa198"))))
;;  '(company-scrollbar-fg ((t (:foreground "#002b36" :background "#839496"))))
;;  '(company-template-field ((t (:background "#7B6000" :foreground "#073642"))))
;;  '(company-tooltip ((t (:background "black" :foreground "DeepSkyBlue1"))))
;;  '(company-tooltip-annotation ((t (:foreground "#93a1a1" :background "#073642"))))
;;  '(company-tooltip-common ((t (:foreground "#93a1a1" :underline t))))
;;  '(company-tooltip-common-selection ((t (:foreground "#93a1a1" :underline t))))
;;  '(company-tooltip-mouse ((t (:background "DodgerBlue4" :foreground "CadetBlue1"))))
;;  '(company-tooltip-selection ((t (:background "DodgerBlue4" :foreground "CadetBlue1"))))
;;  '(header-line ((t (:background "#003366"))))
;;  '(ivy-minibuffer-match-face-1 ((((class color) (background light)) (:background "#555555")) (((class color) (background dark)) (:background "#555555"))))
;;  '(ivy-minibuffer-match-face-2 ((t (:background "#314f30" :weight bold))))
;;  '(ivy-minibuffer-match-face-3 ((t (:background "#48225b" :weight bold))))
;;  '(ivy-minibuffer-match-face-4 ((t (:background "#680a0a" :weight bold))))
;;  '(which-func ((t (:foreground "#8fb28f")))))

;; I don't care to see the splash screen
(setq inhibit-splash-screen t)

;; Hide the scroll bar
;(scroll-bar-mode -1)
;(defvar my-font-size 90)
;; Make mode bar small
;(set-face-attribute 'mode-line nil  :height my-font-size)
;; Set the header bar font
;(set-face-attribute 'header-line nil  :height my-font-size)
;; Set default window size and position
(setq default-frame-alist
      '((top . 0) (left . 0) ;; position
        (width . 110) (height . 90) ;; size
        ))
;; Enable line numbers on the LHS
;; (global-linum-mode -1)
(global-linum-mode t)
(linum-on)
;; Set the font to size 9 (90/10).
;(set-face-attribute 'default nil :height my-font-size)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable which function mode and set the header line to display both the
;; path and the function we're in
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(which-function-mode t)

;; Remove function from mode bar
(setq mode-line-misc-info
      (delete (assoc 'which-func-mode
                     mode-line-misc-info) mode-line-misc-info))


(defmacro with-face
    (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun sl/make-header ()
  "."
  (let* ((sl/full-header (abbreviate-file-name buffer-file-name))
         (sl/header (file-name-directory sl/full-header))
         (sl/drop-str "[...]")
         )
    (if (> (length sl/full-header)
           (window-body-width))
        (if (> (length sl/header)
               (window-body-width))
            (progn
              (concat (with-face sl/drop-str
                                 :background "blue"
                                 :weight 'bold
                                 )
                      (with-face (substring sl/header
                                            (+ (- (length sl/header)
                                                  (window-body-width))
                                              (length sl/drop-str))
                                            (length sl/header))
                                ;; :background "red"
                                :weight 'bold
                                )))
          (concat
           (with-face sl/header
                      ;; :background "red"
                      :foreground "red"
                      :weight 'bold)))
      (concat (if window-system ;; In the terminal the green is hard to read
                  (with-face sl/header
                             ;; :background "green"
                             ;; :foreground "black"
                             :weight 'bold
                             :foreground "#8fb28f"
                             )
                (with-face sl/header
                           ;; :background "green"
                           ;; :foreground "black"
                           :weight 'bold
                           :foreground "blue"
                           ))
              (with-face (file-name-nondirectory buffer-file-name)
                         :weight 'bold
                         ;; :background "red"
                         )))))

(defun sl/display-header ()
  "Create the header string and display it."
  ;; The dark blue in the header for which-func is terrible to read.
  ;; However, in the terminal it's quite nice
  (if window-system
      (custom-set-faces
       '(which-func ((t (:foreground "#8fb28f")))))
    (custom-set-faces
     '(which-func ((t (:foreground "blue"))))))
  ;; Set the header line
  (setq header-line-format

        (list "-"
              '(which-func-mode ("" which-func-format))
              '("" ;; invocation-name
                (:eval (if (buffer-file-name)
                           (concat "[" (sl/make-header) "]")
                         "[%b]")))
              )
        )
  )
;; Call the header line update
(add-hook 'buffer-list-update-hook
          'sl/display-header)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Powerline theme
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; powerline theme where the modes are on the right side.
;; (use-package powerline
;;   :ensure t
;;   :config
;;   (defun powerline-right-theme ()
;;     "Setup a mode-line with major and minor modes on the right side."
;;     (interactive)
;;     (setq-default mode-line-format
;;                   '("%e"
;;                     (:eval
;;                      (let* ((active (powerline-selected-window-active))
;;                             (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
;;                             (mode-line (if active 'mode-line 'mode-line-inactive))
;;                             (face0 (if active 'powerline-active0 'powerline-inactive0))
;;                             (face1 (if active 'powerline-active1 'powerline-inactive1))
;;                             (face2 (if active 'powerline-active2 'powerline-inactive2))
;;                             (separator-left (intern (format "powerline-%s-%s"
;;                                                             (powerline-current-separator)
;;                                                             (car powerline-default-separator-dir))))
;;                             (separator-right (intern (format "powerline-%s-%s"
;;                                                              (powerline-current-separator)
;;                                                              (cdr powerline-default-separator-dir))))
;;                             (lhs (list (powerline-raw "%*" face0 'l)
;;                                        (powerline-buffer-size face0 'l)
;;                                        (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
;;                                        (powerline-raw " ")
;;                                        (funcall separator-left face0 face1)
;;                                        (powerline-narrow face1 'l)
;;                                        (powerline-vc face1)))
;;                             (center (list (powerline-raw global-mode-string face1 'r)
;;                                           (powerline-raw "%4l" face1 'r)
;;                                           (powerline-raw ":" face1)
;;                                           (powerline-raw "%3c" face1 'r)
;;                                           (funcall separator-right face1 face0)
;;                                           (powerline-raw " ")
;;                                           (powerline-raw "%6p" face0 'r)
;;                                           (powerline-hud face2 face1)
;;                                           ))
;;                             (rhs (list (powerline-raw " " face1)
;;                                        (funcall separator-left face1 face2)
;;                                        (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
;;                                          (powerline-raw erc-modified-channels-object face2 'l))
;;                                        (powerline-major-mode face2 'l)
;;                                        (powerline-process face2)
;;                                        (powerline-raw " :" face2)
;;                                        (powerline-minor-modes face2 'l)
;;                                        (powerline-raw " " face2)
;;                                        (funcall separator-right face2 face1)
;;                                        ))
;;                             )
;;                        (concat (powerline-render lhs)
;;                                (powerline-fill-center face1 (/ (powerline-width center) 2.0))
;;                                (powerline-render center)
;;                                (powerline-fill face1 (powerline-width rhs))
;;                                (powerline-render rhs)))))))
;;   (powerline-right-theme)
;;   )

;; use doom modeline instead of powerline
(use-package doom-modeline
      :ensure t
      :defer t
      :hook (after-init . doom-modeline-init))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(dashboard-modify-heading-icons '((recents . "file-text")
                                  (bookmarks . "book")))
(setq dashboard-set-navigator t)
;; Format: "(icon title help action face prefix suffix)"
(setq dashboard-startup-banner 'logo)
(setq dashboard-navigator-buttons
      `(((,(when (icons-displayable-p)
                 (all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0))
              "Homepage" "Browse homepage"
              (lambda (&rest _) (browse-url "gnu.org")))
             (,(when (icons-displayable-p)
                 (all-the-icons-material "restore" :height 1.35 :v-adjust -0.24))
              "Restore" "Restore previous session"
              (lambda (&rest _) (restore-previous-session)))
            ;;  (,(when (icons-displayable-p)
            ;;      (all-the-icons-octicon "tools" :height 1.0 :v-adjust 0.0))
            ;;   "Settings" "Open custom file"
            ;;   (lambda (&rest _) (find-file custom-file)))
            ;;  (,(when (icons-displayable-p)
            ;;      (all-the-icons-material "update" :height 1.35 :v-adjust -0.24))
            ;;   "Update" "Update Centaur Emacs"
            ;;   (lambda (&rest _) (centaur-update)))
             (,(if (icons-displayable-p)
                   (all-the-icons-faicon "question" :height 1.2 :v-adjust -0.1)
                 "?")
              "" "Help (?/h)"
              (lambda (&rest _) (browse-url emacs-china.org))
              font-lock-string-face))))

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(defhydra hydra-org (:color pink
                             :hint nil)
  "
^task^              ^time^              ^Actions^
--------------------------------------------------------------
_n_: new           _s_: start timer    _x_: toggle checkbox
_m_: insert date   _t_: stop timer     _b_: toggle state
                  _S_: set SCHEDULED  _v_: view picture
                  _D_: set DEADLINE   _l_: move left
                                      _r_: move right
"
  ("m" org-time-stamp :exit t)
  ("n" org-meta-return)
  ("t" org-clock-out :exit t)
  ("s" org-clock-in :exit t)
  ("x" org-ctrl-c-ctrl-c :exit t :color red)
  ("b" org-todo :exit t)
  ("S" org-schedule :color purple)
  ("D" org-deadline :color purple :exit t)
  ("c" nil "cancel")
  ("v" org-toggle-inline-images :exit t)
  ("l" org-metaleft :exit t)
  ("r" org-metaright :exit t)
)

(defun my-org-mode-hook()
  (local-set-key (kbd "C-t") 'hydra-org/body))

;; (global-set-key (kbd "C-t") 'hydra-org/body)
(add-hook 'org-mode-hook 'my-org-mode-hook)

;; evil Enable
;(unless (package-installed-p 'evil)
;  (package-install 'evil))
;(require 'evil)
;(setq evil-toggle-key "")   ; remove default evil-toggle-key C-z, manually setup later
;(setq evil-want-C-i-jump nil)   ; don't bind [tab] to evil-jump-forward
;(evil-mode 1)
;; remove all keybindings from insert-state keymap, use emacs-state when editing
;(setcdr evil-insert-state-map nil)
;; ESC to switch back normal-state
;(define-key evil-insert-state-map [escape] 'evil-normal-state)
;; TAB to indent in normal-state
;(define-key evil-normal-state-map (kbd "TAB") 'indent-for-tab-command)
;; ;; Use j/k to move one visual line insted of gj/gk
;; (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
;; (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
;; (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
;; (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)


(provide '.emacs)
;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#282a36" "#ff5555" "#50fa7b" "#f1fa8c" "#61bfff" "#ff79c6" "#8be9fd" "#f8f8f2"])
 '(custom-safe-themes
   '("1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "97db542a8a1731ef44b60bc97406c1eb7ed4528b0d7296997cbb53969df852d6" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "9abe2b502db3ed511fea7ab84b62096ba15a3a71cdb106fd989afa179ff8ab8d" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "b0e446b48d03c5053af28908168262c3e5335dcad3317215d9fdeb8bac5bacf9" default))
 '(exwm-floating-border-color "#242530")
 '(fci-rule-color "#6272a4")
 '(git-gutter:update-interval 5)
 '(highlight-tail-colors
   ((("#2c3e3c" "#2a3b2e" "green")
     . 0)
    (("#313d49" "#2f3a3b" "brightcyan")
     . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#1E2029" "#bd93f9"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1E2029" "#50fa7b"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1E2029" "#565761"))
 '(objed-cursor-color "#ff5555")
 '(package-selected-packages
   '(company-box company-quickhelp-terminal company-quickhelp all-the-icons-completion all-the-icons-dired all-the-icons-gnus all-the-icons-ibuffer all-the-icons-ivy all-the-icons-ivy-rich neotree melancholy-theme company-prescient hydra monokai-theme xclip doom-modeline doom-themes 2048-game ace-flyspell powerline zzz-to-char yasnippet-snippets yapfify yaml-mode writegood-mode window-numbering which-key wgrep web-mode vlf use-package string-inflection sourcerer-theme realgud rainbow-delimiters phi-autopair origami multiple-cursors modern-cpp-font-lock markdown-mode magit-gerrit json-mode hungry-delete google-c-style git-gutter flyspell-correct-ivy flymake flycheck-ycmd flycheck-pyflakes flex-compile flex-autopair elpy edit-server cuda-mode counsel-etags company-ycmd company-statistics company-jedi cmake-mode clang-format beacon auto-package-update auctex async))
 '(pdf-view-midnight-colors (cons "#f8f8f2" "#282a36"))
 '(rustic-ansi-faces
   ["#282a36" "#ff5555" "#50fa7b" "#f1fa8c" "#61bfff" "#ff79c6" "#8be9fd" "#f8f8f2"])
 '(vc-annotate-background "#282a36")
 '(vc-annotate-color-map
   (list
    (cons 20 "#50fa7b")
    (cons 40 "#85fa80")
    (cons 60 "#bbf986")
    (cons 80 "#f1fa8c")
    (cons 100 "#f5e381")
    (cons 120 "#face76")
    (cons 140 "#ffb86c")
    (cons 160 "#ffa38a")
    (cons 180 "#ff8ea8")
    (cons 200 "#ff79c6")
    (cons 220 "#ff6da0")
    (cons 240 "#ff617a")
    (cons 260 "#ff5555")
    (cons 280 "#d45558")
    (cons 300 "#aa565a")
    (cons 320 "#80565d")
    (cons 340 "#6272a4")
    (cons 360 "#6272a4")))
 '(vc-annotate-very-old-color nil))

;; (setq flycheck-clang-include-path(list "/usr/local/include")
(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'c++-mode-hook 'ycmd-mode)


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Awesome-tabbar
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  (require 'awesome-tab)

;; (awesome-tab-mode t)

;; (global-set-key (kbd "C-t") 'awesome-tab-forward)

(electric-pair-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(which-func ((t (:foreground "blue")))))
