;;; early-init.el --- Early Emacs config  -*- lexical-binding: t; -*-

(setq my/original-gc-cons-threshold  gc-cons-threshold
      gc-cons-threshold              most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold my/original-gc-cons-threshold)))

(add-to-list 'load-path (locate-user-emacs-file "lisp/"))

;; package manager: Elpaca (https://github.com/progfolio/elpaca)
(setq load-prefer-newer                t
      package-enable-at-startup        nil
      use-package-verbose              nil
      use-package-always-ensure        t
      use-package-always-defer         t
      use-package-enable-imenu-support t)
(load "init-elpaca.el" nil :nomessage)
(elpaca elpaca-use-package (elpaca-use-package-mode))

(use-package auto-compile
  :custom (auto-compile-display-buffer nil)
  :init
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; https://github.com/emacscollective/no-littering#native-compilation-cache
(startup-redirect-eln-cache
   (convert-standard-filename
  (expand-file-name  "var/eln-cache/" user-emacs-directory)))

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
        (font . "Iosevka-14")))         ; font
(setq custom-safe-themes t)             ; to be overwitten by custom.el
(use-package batppuccin-themes
  :vc (:url "https://github.com/bbatsov/batppuccin-emacs" :rev :newest)
  :init (load-theme 'batppuccin-frappe :no-confirm))
(use-package auto-dark
  :custom (auto-dark-themes '((batppuccin-mocha) (batppuccin-frappe)))
  :init (auto-dark-mode))
