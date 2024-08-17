;; init-emacs.el - Emacs-specific misc config  -*- lexical-binding: t; -*-

(setopt					; make Emacs quieter
 confirm-kill-processes nil
 use-short-answers t
 inhibit-startup-screen t
 initial-scratch-message ""
 ad-redefinition-action 'accept
 load-prefer-newer t)                   ; weird that it's not default

(require 'server)                       ; emacs --daemon
(unless (server-running-p) (server-start))

(use-package f)
(use-package general)
(use-package defrepeater)
(elpaca-wait)

(setopt custom-file		     ; store customizations separately
      (expand-file-name "custom.el"
                        (f-join user-emacs-directory "lisp/")))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file)))

;;;;;;;;;;;;;;
;; keybinds ;;
;;;;;;;;;;;;;;

(general-def
  "C-x C-m"   'execute-extended-command ; more convenient than M-x
  "C-x m"     'execute-extended-command
  "C-x M-c"   'restart-emacs
  "C-x C-M-c" 'save-buffers-kill-emacs)

;;;;;;;;;;;;;;;;;;;;;;
;; extenal packages ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package no-littering               ; must be set before load path
  :init
  (setopt no-littering-etc-directory (f-join user-emacs-directory "lisp/"))
  :custom
  (create-lockfiles nil)
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory))
  (no-littering-theme-backups))


(add-to-list 'load-path (f-join user-emacs-directory "lisp/"))


(provide 'init-emacs)
