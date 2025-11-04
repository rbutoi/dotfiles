;; init-emacs.el - Emacs-specific misc config  -*- lexical-binding: t; -*-

(setopt
 confirm-kill-processes     nil
 use-short-answers          t
 initial-scratch-message    ""
 ;; use-package-compute-statistics t       ; for (use-package-report)
 load-prefer-newer          t)

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
  :config
  (no-littering-theme-backups)
  ;; load path setup
  (add-to-list 'load-path no-littering-etc-directory))
(elpaca-wait)


(provide 'init-emacs)
