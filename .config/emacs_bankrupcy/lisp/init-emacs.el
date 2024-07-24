;; init-emacs.el - Emacs-specific misc config  -*- lexical-binding: t; -*-

(use-package general)                   ; keybinds
(elpaca-wait)

(setq custom-file		     ; store customizations separately
      (expand-file-name "custom.el"
                        (f-join user-emacs-directory "lisp/")))
(load custom-file)

(require 'server)                       ; emacs --daemon
(unless (server-running-p) (server-start))

(general-def
  "C-x C-m"   'execute-extended-command ; more convenient than M-x
  "C-x m"     'execute-extended-command
  "C-x M-c"   'restart-emacs
  "C-x C-M-c" 'save-buffers-kill-emacs)

(setq					; make Emacs quieter
 confirm-kill-processes nil
 use-short-answers t
 inhibit-startup-screen t
 initial-scratch-message ""
 ad-redefinition-action 'accept)

(use-package no-littering               ; must be before load path
  :init
  (setq no-littering-etc-directory (f-join user-emacs-directory "lisp/"))
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory))
  (no-littering-theme-backups))


(provide 'init-emacs)
