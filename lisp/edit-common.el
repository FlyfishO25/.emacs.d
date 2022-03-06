;;; edit-common.el --- setup basic edit configures. -*- lexical-binding: t; -*-

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup basic edit configures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

;; move window
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
;; (tool-bar-mode 0)                     
;; (menu-bar-mode 0)
;; (when (display-graphic-p) (scroll-bar-mode 0))

(electric-pair-mode 1)
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)
(show-paren-mode 1)
(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
	(t (save-excursion
	     (ignore-errors (backward-up-list))
	     (funcall fn)))))
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

(when (string= system-type "darwin")
  (unless (executable-find "gls")
    (message "We can not detect gls program in this machine, maybe you need to install homebrew and then install it."))
  (setq dired-use-ls-dired t
        insert-directory-program "/usr/local/bin/gls"
        dired-listing-switches "-aBhl --group-directories-first"))

(use-package anzu
  :diminish
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode))

(use-package multiple-cursors
  :defer 3
  :bind (("M-n" . mc/mark-next-like-this)
         ("M-p" . mc/mark-previous-like-this)
         ("C-c a" . mc/mark-all-like-this)
         ("C-c e" . mc/edit-lines))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; avy: always fast jump to char inside the current view buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package avy
  :defer 2
  :bind (("M-c" . avy-goto-char)
         ("M-s" . avy-goto-word-1))
  ;; Set keys for Dvorak mode instead of qwerty
  ;; :init (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s
  ;;                           ?A ?O ?E ?U ?I ?D ?H ?T ?N ?S
  ;;                           ?p ?y ?f ?g ?c ?r ?l
  ;;                           ?P ?Y ?F ?G ?C ?R ?L))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zzz-to-char: replaces the built-in zap-to-char with avy-like
;;              replacement options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package zzz-to-char
  :defer 2
  :bind ("M-z" . zzz-up-to-char))

;; Treat undo history as a tree ;; from https://github.com/seagle0128/.emacs.d/blob/e840ab62fd5f1a8df9818d0678e7413145e4c8d3/lisp/init-edit.el#L320
(use-package undo-tree
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-enable-undo-in-region nil
        undo-tree-auto-save-history nil)

  ;; HACK: keep the diff window
  (with-no-warnings
    (make-variable-buffer-local 'undo-tree-visualizer-diff)
    (setq-default undo-tree-visualizer-diff t)))

;;; edit-common.el ends here
