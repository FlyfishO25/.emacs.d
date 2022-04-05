;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq use-package-verbose t)
(setq package-enable-at-startup nil)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; ban ads
(defun display-startup-echo-area-message ()
  "Re-define this funciton to ban GNU's ad."
  (message nil))

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; (when (featurep 'ns)
;;   (push '(ns-transparent-titlebar . t) default-frame-alist))

(defun ns-auto-titlebar-set-frame (frame &rest _)
  "Set ns-appearance frame parameter for FRAME to match its background-mode parameter."
  (when (display-graphic-p frame)
    (let ((mode (frame-parameter frame 'background-mode)))
      (modify-frame-parameters frame `((ns-transparent-titlebar . t) (ns-appearance . ,mode))))))

(defun ns-auto-titlebar-set-all-frames (&rest _)
  "Set ns-appearance frame parameter for all frames to match their background-mode parameter."
  (mapc 'ns-auto-titlebar-set-frame (frame-list)))

(when (eq system-type 'darwin) (progn
                                 (add-hook 'after-init-hook 'ns-auto-titlebar-set-all-frames)
                                 (add-hook 'after-make-frame-functions 'ns-auto-titlebar-set-frame)
                                 (advice-add 'frame-set-background-mode :after 'ns-auto-titlebar-set-frame)
                                 ))
