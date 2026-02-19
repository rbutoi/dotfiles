;;; early-init.el --- Early Emacs config  -*- lexical-binding: t; -*-

(setq use-package-verbose            t
      my/original-gc-cons-threshold  gc-cons-threshold
      gc-cons-threshold              most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (when use-package-verbose
              (message "Emacs ready in %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time (time-subtract after-init-time before-init-time)))
                       gcs-done)
              ;; (use-package-report)
              )
            (setq gc-cons-threshold my/original-gc-cons-threshold)))

(add-to-list 'load-path (locate-user-emacs-file "lisp/"))
(setq load-prefer-newer t)

;; package manager: Elpaca (https://github.com/progfolio/elpaca)
(setq package-enable-at-startup        nil
      use-package-always-ensure        t
      use-package-always-defer         t
      ;; use-package-compute-statistics   use-package-verbose ; for (use-package-report)
      use-package-enable-imenu-support t)
(load (locate-user-emacs-file "lisp/init-elpaca.el") nil :nomessage)
(elpaca elpaca-use-package (elpaca-use-package-mode))

;; https://github.com/emacscollective/no-littering#native-compilation-cache
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;; UI settings before initial frame is created
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq initial-frame-alist '((width . 130) (height . 50)))

(use-package doom-themes :init (load-theme 'doom-dark+ t))
(add-to-list 'default-frame-alist '(font . "SF Mono-14"))

(push '(tool-bar-lines . 0)   default-frame-alist) ; speedups
(push '(vertical-scroll-bars) default-frame-alist)
