;;; init-const.el --- define constants	-*- lexical-binding: t -*-

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
;; Define some constants used in flymacs.

;;; Code:

(defconst flymacs-files-to-compile
  '((concat user-emacs-directory "config-user.el")
    (concat user-emacs-directory "site-lisp/xah-fly-keys/xah-fly-keys.el")
    (concat user-emacs-directory "site-lisp/xah-fly-keys.el")
    (concat user-emacs-directory "init.el")
    (concat user-emacs-directory "site-lisp/ox-html5presentation.el")
    )
  )

(provide 'init-const)

;;; init-const.el ends here
