;;; init-emacs.el --- Emacs-specific misc config  -*- lexical-binding: t; -*-

(setopt
 confirm-kill-processes   nil
 use-short-answers        t
 inhibit-startup-screen   t
 initial-scratch-message  "")

(use-package server :ensure nil
  :config (unless (server-running-p) (server-start)))

(use-package system-packages)

(use-package exec-path-from-shell
  :disabled                             ; hmm
  :config
  (dolist (var '("RIPGREP_CONFIG_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package general      :demand t :ensure (:wait t))

(use-package no-littering :demand t
  :init   (setopt no-littering-etc-directory (file-name-concat user-emacs-directory "lisp/"))
  ;; :custom (create-lockfiles nil)
  :config
  (no-littering-theme-backups)
  (setopt custom-file (no-littering-expand-etc-file-name "custom.el"))
  (load custom-file nil :nomessage))

;; OS-specific
(when (eq system-type 'darwin)
  (setopt mac-option-modifier       'meta
          mac-command-modifier      'super
          insert-directory-program  "gls" ; gnu coreutils
          manual-program            "gman"))

;; Package updates
(use-package async)
(use-package elpaca-daily-update :ensure nil
  :hook (elpaca-after-init . elpaca-daily-update-init))


(provide 'init-emacs)
