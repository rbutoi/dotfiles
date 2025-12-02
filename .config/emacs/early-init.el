;; early-init.el - Early Emacs config  -*- lexical-binding: t; -*-

(setq load-prefer-newer t)

;; package manager: Elpaca (https://github.com/progfolio/elpaca)
(setq package-enable-at-startup nil)
(load (locate-user-emacs-file "lisp/init-elpaca.el"))
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t))

;; UI speedups
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq initial-frame-alist '((width . 130) (height . 50))) ; before any frames are created
