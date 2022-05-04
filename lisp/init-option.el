;;; init-user.el --- Load user configures. -*- lexical-binding: t -*-

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
;; Load user configures, just require.

;;; Code:

(unless (file-exists-p (expand-file-name "config-user-option.el" user-emacs-directory))
  (copy-file (expand-file-name "config-user-option.example.el" user-emacs-directory)
             (expand-file-name "config-user-option.el"         user-emacs-directory)))

(load (expand-file-name "config-user-option" user-emacs-directory))

(unless (boundp 'my:compile-command)
  (setq my:compile-command ""))

(unless (boundp 'flymacs-cpp-google-style)
  (setq flymacs-cpp-google-style nil))

(unless (boundp 'flymacs-server)
  (setq flymacs-server nil))

(unless (boundp 'display-icon)
  (setq display-icon t))

(unless (boundp 'completion-style)
  (setq completion-style 'childframe)
  )

(unless (boundp 'flymacs-ui)
  (setq flymacs-ui 'rich))

(unless (boundp 'centaur-lsp)
  (setq centaur-lsp 'lsp-mode))
;;; init-user.el ends here
