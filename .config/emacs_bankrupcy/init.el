;; init.el - Emacs config  -*- lexical-binding: t; -*-

;; package manager: Elpaca (https://github.com/progfolio/elpaca)
(load (expand-file-name "lisp/init-elpaca.el" user-emacs-directory))
(elpaca elpaca-use-package
        (elpaca-use-package-mode)
        (setq use-package-always-ensure t))

(use-package f)
(elpaca-wait)
(add-to-list 'load-path (f-join user-emacs-directory "lisp/"))

(require 'init-emacs)

;; (require 'init-fns) TODO
(load "~/.config/emacs/lisp/config-fns.el")
(require 'init-ui)
(require 'init-buffers)
(require 'init-editing)
(require 'init-programming)

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;


(recentf-mode)
