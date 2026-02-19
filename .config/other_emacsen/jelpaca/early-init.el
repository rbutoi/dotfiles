;;; early-init.el --- Early Emacs config  -*- lexical-binding: t; -*-

;; package manager: Elpaca (https://github.com/progfolio/elpaca)
(setq package-enable-at-startup nil)
(load (locate-user-emacs-file "init-elpaca.el"))
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t))
