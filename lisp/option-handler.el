;;; option-handler.el --- handle CLI options for flymacs

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
;; (require 'option-handler)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(defun flymacs-handle-normal-startup ()
  "Handle startup when -minimal is not passed to Emacs."
                                        ; basic setup
  (unless (boundp 'flymacs--initialized)

    (loadpkg 'ui-configure t)
    (loadpkg 'ui-modeline t)
    ;; (loadpkg 'ui-tab)
    (loadpkg 'ui-dashboard)
    (loadpkg 'ui-theme t)
    (loadpkg 'ui-flycheck)
    (loadpkg 'ui-cnfont t)

    (loadpkg 'edit-common t)
    (loadpkg 'edit-autosave t)
    (loadpkg 'edit-keybinds t)
    
    (loadpkg 'init-ivy t)
    (loadpkg 'init-hydra)
                                        ; code editing

    (loadpkg 'lang-python)
    (loadpkg 'lang-c)

                                        ; code completion

    (loadpkg 'complete-company t)
    (loadpkg 'complete-lsp)

                                        ; other modes
    (loadpkg 'init-org)
    (add-hook 'org-mode-hook (lambda () (loadpkg 'init-roam)))
    (loadpkg 'reading)
    (loadpkg 'init-git)
                                        ; games
    (loadpkg 'init-games)
    (flymacs-post-install)
    )
  )

(defun flymacs-handle-minimal-startup ()
  "Handle startup when -minimal OPTION is passed to Emacs."
  (setq flymacs--initialized t)

  (loadpkg 'ui-configure t)
  (loadpkg 'ui-modeline t)

  (loadpkg 'ui-theme t)
  
  (loadpkg 'edit-common t)
  (loadpkg 'edit-autosave t)
  (loadpkg 'edit-keybinds t)
  
  (loadpkg 'init-ivy t)
                                        ; code completion

  (loadpkg 'complete-company t)
                                        ; other modes
  (loadpkg 'init-org)
  (loadpkg 'init-git)

  (flymacs-post-install)
  )

(provide 'option-handler)
