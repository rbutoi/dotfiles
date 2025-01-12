;; init-emacs.el - Emacs-specific misc config  -*- lexical-binding: t; -*-

(setopt					; make Emacs quieter
 confirm-kill-processes nil
 use-short-answers t
 inhibit-startup-screen t
 initial-scratch-message ""
 ad-redefinition-action 'accept
 load-prefer-newer t)                   ; weird that it's not default

(require 'server) (unless (server-running-p) (server-start)) ; emacs --daemon

;;;;;;;;;;;;;;;;;;;;;;
;; extenal packages ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package f)
(use-package general) (use-package defrepeater) ; keybinds
(elpaca-wait)
(use-package no-littering               ; must be set before load path
  :init   (setopt no-littering-etc-directory (f-join user-emacs-directory "lisp/"))
  :custom (create-lockfiles nil)
  :config (no-littering-theme-backups))
(elpaca-wait)
(add-to-list 'load-path no-littering-etc-directory)


(provide 'init-emacs)
