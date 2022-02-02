;;; init-games.el --- Initialize the game packages.  -*- lexical-binding: t -*-

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
;; Install games packages, don't need to do anything.

;;; Code:

(require 'url)

(unless (boundp 'flymacs-games)
  (setq flymacs-games nil))

(use-package pacmacs
  :if flymacs-games
  :commands (pacmacs-start))

(use-package 2048-game
  :if flymacs-games
  :commands (2048-game))

(use-package snow
  :if flymacs-games
  :commands (snow))

(use-package zone
  ;; @see https://github.com/MatthewZMD/.emacs.d#org726c422
  :ensure nil
  :defer 5
  :config
  ;; (zone-when-idle 600) ; in seconds
  (defun zone-choose (pgm)
    "Choose a PGM to run for `zone'."
    (interactive
     (list
      (completing-read
       "Program: "
       (mapcar 'symbol-name zone-programs))))
    (let ((zone-programs (list (intern pgm))))
      (zone))))

(if flymacs-games
    (progn
      (unless (file-exists-p (expand-file-name "site-lisp/wordel.el" user-emacs-directory))
        (url-copy-file "https://raw.fastgit.org/progfolio/wordel/main/wordel.el" (expand-file-name "site-lisp/wordel.el" user-emacs-directory))
        (unless (file-directory-p (expand-file-name "site-lisp/words/" user-emacs-directory))
          (make-directory (expand-file-name "site-lisp/words/" user-emacs-directory)))
        (url-copy-file "https://raw.fastgit.org/progfolio/wordel/main/words/scrabble" (expand-file-name "site-lisp/words/scrabble" user-emacs-directory))
        (url-copy-file "https://raw.fastgit.org/progfolio/wordel/main/words/wordle" (expand-file-name "site-lisp/words/wordle" user-emacs-directory))
        )
      (require 'wordel)
      )
  )

;;; init-games.el ends here
