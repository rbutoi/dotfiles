;; init.el - Emacs config  -*- lexical-binding: t; -*-

;; package manager: Elpaca (https://github.com/progfolio/elpaca)
(load (expand-file-name "lisp/init-elpaca.el" user-emacs-directory))
(elpaca elpaca-use-package
        (elpaca-use-package-mode)
        (setq use-package-always-ensure t))

;; Set load path and initialize basic Emacs settings
(load (expand-file-name "lisp/init-emacs.el" user-emacs-directory))

(require 'init-fns)
(require 'init-ui)
(require 'init-buffers)
(require 'init-editing)
(require 'init-programming)
(require 'init-langs)
(require 'init-ext)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d9a947788a4c5f7051c4ad3a3e0e9d76218209899683d3e9ed1e2aa6cd10d462" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
