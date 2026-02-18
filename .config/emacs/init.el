;;; init.el --- Emacs config  -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-concat user-emacs-directory "lisp/"))
(require 'init-emacs)
(require 'init-fns)

(require 'init-ui)
(require 'init-buffers)
(require 'init-completion)
(require 'init-editing)
(require 'init-programming)
(require 'init-eglot)
(require 'init-langs)
(require 'init-vcs)
(require 'init-ext)

;; (setq custom-file (locate-library "custom"))
;; (load custom-file)

(message "(emacs-init-time): %s" (emacs-init-time))
