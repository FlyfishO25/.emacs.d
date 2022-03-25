;;; ui-dashboard.el --- setup dashboard. -*- lexical-binding: t; -*-

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
;; Setup dashboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(use-package dashboard
  :diminish dashboard-mode
  :functions (all-the-icons-faicon
              all-the-icons-material)
  :custom-face (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
  :init
  (setq dashboard-banner-logo-title "F L Y M A C S"
        dashboard-startup-banner 'logo
        dashboard-page-separator "\n\f\n"
        dashboard-center-content t
        dashboard-show-shortcuts nil
        dashboard-items '((recents  . 10)
                          (bookmarks . 5)
                          )

        dashboard-set-init-info t
        dashboard-heading-icons '((recents   . "file-text")
                                  (bookmarks . "bookmark")
                                  (agenda    . "calendar")))
  (setq initial-buffer-choice (lambda () (if (get-buffer "*dashboard*")
                                             (get-buffer "*dashboard*")
                                              (get-buffer "*scratch*"))
  (dashboard-setup-startup-hook)
  )

;;; ui-dashboard.el ends here.
