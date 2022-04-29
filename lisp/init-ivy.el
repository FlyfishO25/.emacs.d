;;; init-ivy.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2022 FlyfishO25

;; Author: FlyfishO25 <markzhou0125@gmail.com>
;; URL: https://github.com/FlyfishO25/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;; Ivy config

;;; Code:

(use-package ivy
  :hook (after-init . ivy-mode)
  :commands (ivy-mode)
  :config
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-wrap t
        ivy-on-del-error-function #'ignore
        
        ;; Show #/total when scrolling buffers
        ivy-count-format "%d/%d "
        ivy-ignore-buffers '("\\` " "\\`\\*tramp/" "\\`\\*xref" "\\`\\*helpful "
                             "\\`\\*.+-posframe-buffer\\*"))
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  )

(use-package counsel
  :ensure t
  :after ivy
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
    (warn "\nWARNING: Could not find the ripgrep executable. It 
          is recommended you install ripgrep.")
    )
  )

(use-package ctrlf
  ;; use ctrlf as a replace of swiper
  :demand
  :custom-face
  (ctrlf-highlight-active ((t (:weight bold :foreground "medium blue" :background "#5AC896"))))
  :config
  (ctrlf-mode)
)

;; Use universal ctags to build the tags database for the project.
;; When you first want to build a TAGS database run 'touch TAGS'
;; in the root directory of your project.
(use-package counsel-etags
  :ensure t
  :after counsel
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

(use-package prescient
  :commands prescient-persist-mode
  :init (prescient-persist-mode 1))

(use-package all-the-icons-ivy-rich
  :hook (after-init . all-the-icons-ivy-rich-mode)
  :if (icons-displayable-p))

(use-package ivy-rich
  :hook (after-init . ivy-rich-mode)
  )

(use-package ivy-prescient
  :commands ivy-prescient-re-builder
  :custom-face
  (ivy-minibuffer-match-face-1 ((t (:foreground ,(face-foreground 'font-lock-doc-face nil t)))))
  :init
  ;; See https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-ivy.el#397
  (defun ivy-prescient-rebuilder (str)
    "Generate an regexp for STR by `ivy-prescient-re-builder'."
    (let ((prescient-filter-method '(literal regexp)))
      (ivy-prescient-re-builder str)
      ))
  (setq ivy-prescient-retain-classic-highlighting t
        ivy-re-builders-alist
        '((counsel-ag . ivy-prescient-rebuilder)
          (counsel-pt . ivy-prescient-rebuilder)
          (counsel-grep . ivy-prescient-rebuilder)
          (counsel-fzf . ivy-prescient-rebuilder)
          (counsel-imenu . ivy-prescient-rebuilder)
          (counsel-yank-pop . ivy-prescient-rebuilder)
          (swiper . ivy-prescient-rebuilder)
          (swiper-isearch . ivy-prescient-rebuilder)
          (swiper-all . ivy-prescient-rebuilder)
          (lsp-ivy-workspace-symbol . ivy-prescient-rebuilder)
          (lsp-ivy-global-workspace-symbol . ivy-prescient-rebuilder)
          (insert-char . ivy-prescient-rebuilder)
          (counsel-unicode-char . ivy-prescient-rebuilder)
          (t . ivy-prescient-re-builder))
        ivy-prescient-sort-commands
        '(:not swiper swiper-isearch ivy-switch-buffer
               lsp-ivy-workspace-symbol ivy-resume ivy--restore-session
               counsel-grep counsel-git-grep counsel-rg counsel-ag
               counsel-ack counsel-fzf counsel-pt counsel-imenu
               counsel-org-capture counsel-outline counsel-org-goto
               counsel-load-theme counsel-yank-pop
               counsel-recentf counsel-buffer-or-recentf))
  (ivy-prescient-mode 1)
  )

(defvar ivy-fly-commands
  '(query-replace-regexp
    flush-lines keep-lines ivy-read
    swiper swiper-backward swiper-all
    swiper-isearch swiper-isearch-backward
    lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol
    counsel-grep-or-swiper counsel-grep-or-swiper-backward
    counsel-grep counsel-ack counsel-ag counsel-rg counsel-pt))

(defvar ivy-fly-back-commands
  '(self-insert-command
    ivy-forward-char ivy-delete-char delete-forward-char kill-word kill-sexp
    end-of-line mwim-end-of-line mwim-end-of-code-or-line mwim-end-of-line-or-code
    yank ivy-yank-word ivy-yank-char ivy-yank-symbol counsel-yank-pop))

(defun ivy-fly-back ()
  (cond ((and (memq last-command ivy-fly-commands)
              (equal (this-command-keys-vector) (kbd "M-p")))
         (or (memq this-command ivy-fly-back-commands)
             (equal (this-command-keys-vector) (kbd "M-n")))
         )
        ((delete-region (point) (point-max))
         (when (memq this-command '(ivy-forward-char
                                    ivy-delete-char delete-forward-char
                                    kill-word kill-sexp
                                    end-of-line mwim-end-of-line
                                    mwim-end-of-code-or-line
                                    mwim-end-of-line-or-code))
           (insert (ivy-cleanup-string ivy-text))
           (when (memq this-command '(ivy-delete-char
                                      delete-forward-char
                                      kill-word kill-sexp))
             (beginning-of-line))))))

(defun ivy-fly-hist ()
  (when (memq this-command ivy-fly-commands)
    (insert (propertize
             (save-excursion
	       (set-buffer (window-buffer (minibuffer-selected-window)))
	       (ivy-thing-at-point))
             'face 'shadow))
    (add-hook 'pre-command-hook 'ivy-fly-back nil t)
    (beginning-of-line)))

(add-hook 'minibuffer-setup-hook #'ivy-fly-hist)
(add-hook 'minibuffer-exit-hook
          (lambda ()
            (remove-hook 'pre-command-hook 'ivy-fly-back t)))

;;; init-ivy.el ends here
