;;; init-misc.el --- initialization miscellaneous -*- lexical-binding: t; -*-

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


;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; miscellaneous things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(use-package no-littering               ; Keep .emacs.d clean
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(load custom-file 'noerror)

(defun launch-separate-emacs-in-terminal ()
  "Launch another Emacs process in terminal."
  (suspend-emacs "fg ; emacs -nw"))

(defun launch-separate-emacs-under-gui ()
  "Launch another Emacs process under GUI."
  (call-process "sh" nil nil nil "-c" "emacs &"))

(defun restart-emacs ()
  "Restart Emacs from within Emacs."
  (interactive)
  (if (eq system-type 'windows-nt)
      (message "Sorry, this function does not support windows. QAQ")
    (let ((kill-emacs-hook (append kill-emacs-hook (list (if (display-graphic-p)
                                                             #'launch-separate-emacs-under-gui
                                                           #'launch-separate-emacs-in-terminal)))))
      (save-buffers-kill-emacs))))

(fset 'yes-or-no-p 'y-or-n-p)

(use-package winner
  :ensure nil
  :custom
  (winner-boring-buffers
   '("*Completions*"
     "*Compile-Log*"
     "*inferior-lisp*"
     "*Fuzzy Completions*"
     "*Apropos*"
     "*Help*"
     "*cvs*"
     "*Buffer List*"
     "*Ibuffer*"
     "*esh command on file*"))
  :config
  (winner-mode 1))

(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t)
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;;; init-misc.el ends here
