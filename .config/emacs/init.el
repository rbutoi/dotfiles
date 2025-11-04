;; init.el - Emacs config  -*- lexical-binding: t; -*-

(load (locate-user-emacs-file "lisp/init-emacs.el")) ; set load path and initialize basic Emacs settings
(require 'init-fns)                                  ; some fns used later

(require 'init-ui)
(require 'init-buffers)
(require 'init-editing)
(require 'init-programming)
(require 'init-langs)
(require 'init-ext)

(setq custom-file (locate-user-emacs-file "lisp/custom.el"))
(load custom-file)

(message "(emacs-init-time): %s" (emacs-init-time))
