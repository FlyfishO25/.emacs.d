;;; init-const.el --- define variable	-*- lexical-binding: t -*-

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
;; Define some variable used in flymacs that can be changed by user.

;;; Code:

(defvar flymacs-etc-dir
  "Directory for flymacs to save local storage

Use this directory to save long-term files or binaries that seldom change. Must ends with a slash."
  (concat flymacs-local-dir "etc/")
  )

(defvar flymacs-cache-dir
  "Directory for flymacs to save local storage

Use this directory to save short-term files like connection cache. Must ends with a slash."
  (concat flymacs-local-dir "cache/"))

(defvar flymacs-private-dir
  "Directory to save user configures. Must ends with a slash."
  (expand-file-name "~/.config/flymacs/"))

(provide 'init-vars)
;;; init-vars.el ends here
