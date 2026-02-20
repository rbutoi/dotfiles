;;; early-init.el --- Early Emacs config  -*- lexical-binding: t; -*-

(setq my/original-gc-cons-threshold  gc-cons-threshold
      gc-cons-threshold              most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold my/original-gc-cons-threshold)))

(add-to-list 'load-path (locate-user-emacs-file "lisp/"))
(setq load-prefer-newer noninteractive)

;; package manager: Elpaca (https://github.com/progfolio/elpaca)
(setq package-enable-at-startup        nil
      use-package-verbose              t
      use-package-always-ensure        t
      use-package-always-defer         t
      use-package-enable-imenu-support t)
(load "init-elpaca.el" nil :nomessage)
(elpaca elpaca-use-package (elpaca-use-package-mode))

;; https://github.com/emacscollective/no-littering#native-compilation-cache
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;; UI settings before initial frame is created
(setq initial-frame-alist               ; frame size and position
      (or (ignore-errors
            (with-temp-buffer
              (insert-file-contents (locate-user-emacs-file "var/frame-geometry.el"))
              (read (current-buffer))))
          '((width . 130) (height . 50))))
(setq default-frame-alist
      '((tool-bar-lines . 0)            ; speedups
        (vertical-scroll-bars)
        (ns-transparent-titlebar . t)   ; macOS: dark titlebar
        (ns-appearance . dark)
        (font . "Iosevka-15")))         ; font
(use-package doom-themes                ; theme
  :init (load-theme 'doom-dark+ t))     
