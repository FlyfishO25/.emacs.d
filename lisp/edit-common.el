;;; -*- lexical-binding: t; -*-
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
(setq auto-save-default nil)
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

(electric-pair-mode 1)
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

;; Remove trailing white space upon saving
;; Note: because of a bug in EIN we only delete trailing whitespace
;; when not in EIN mode.
;; (add-hook 'before-save-hook
;;           (lambda ()
;;             (when (not (derived-mode-p 'ein:notebook-multilang-mode))
;;               (delete-trailing-whitespace))))

;; (global-auto-revert-mode 1)

;; Auto-wrap at 80 characters
(setq-default auto-fill-function 'do-auto-fill)
(setq-default fill-column 80)
(turn-on-auto-fill)
;; Disable auto-fill-mode in programming mode
(add-hook 'prog-mode-hook (lambda () (auto-fill-mode -1)))

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

                                        ; shortcuts

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
;; Copy and paste
(global-set-key (kbd "C-c M-w") 'osx-copy)
(global-set-key (kbd "C-c C-y") 'osx-paste)
;; complete the code
;;(global-set-key (kbd "C-c TAB") 'company-complete)

(provide 'edit-common)
