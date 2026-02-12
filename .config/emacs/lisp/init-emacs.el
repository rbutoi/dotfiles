;;; init-emacs.el --- Emacs-specific misc config  -*- lexical-binding: t; -*-

(setopt
 confirm-kill-processes     nil
 use-short-answers          t
 ;; use-package-compute-statistics t       ; for (use-package-report)
 inhibit-startup-screen     t
 initial-scratch-message    "")

(server-start)

(use-package system-packages)

(use-package exec-path-from-shell
  :config
  (dolist (var '("RIPGREP_CONFIG_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package f)
(use-package general)                   ; keybinds
(elpaca-wait)
(use-package no-littering               ; must be set before load path
  :init   (setopt no-littering-etc-directory (f-join user-emacs-directory "lisp/"))
  :custom (create-lockfiles nil)
  :config
  (no-littering-theme-backups)
  ;; load path setup
  (add-to-list 'load-path no-littering-etc-directory))
(elpaca-wait)

;; OS-specific
(when (eq system-type 'darwin)
  (setopt mac-option-modifier       'meta
          mac-command-modifier      'super
          insert-directory-program  "gls" ; gnu coreutils
          manual-program            "gman"))

;; Package updates
(use-package async)
(use-package elpaca-daily-update :after async :ensure nil)


(provide 'init-emacs)
