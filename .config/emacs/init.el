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
