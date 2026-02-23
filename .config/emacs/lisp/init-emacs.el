;;; init-emacs.el --- Better defaults  -*- lexical-binding: t; -*-

(use-package server :ensure nil         ; for emacsclient
  :config (unless (server-running-p) (server-start)))

(use-package no-littering :demand t     ; clean up emacs config dir
  :init (setopt no-littering-etc-directory (file-name-concat user-emacs-directory "lisp/"))
  :custom (create-lockfiles nil)
  :config
  (no-littering-theme-backups)
  (setopt custom-file (no-littering-expand-etc-file-name "custom.el"))
  (load custom-file nil :nomessage))

(use-package async)
(use-package elpaca-daily-update :ensure nil ; check for package updates
  :hook (elpaca-after-init . elpaca-daily-update-init))

(use-package system-packages)           ; installing system packages from Emacs

(setopt confirm-kill-processes nil
        use-short-answers t)
(when (eq system-type 'darwin)
  (setopt mac-option-modifier       'meta
          mac-command-modifier      'super
          insert-directory-program  "gls" ; gnu coreutils
          manual-program            "gman"))

(when use-package-verbose
  (add-hook 'emacs-startup-hook
            (lambda ()
              (message "Emacs ready in %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time (time-subtract after-init-time before-init-time)))
                       gcs-done))))

;; (use-package exec-path-from-shell
;;   :disabled                             ; hmm
;;   :config
;;   (dolist (var '("RIPGREP_CONFIG_PATH"))
;;     (add-to-list 'exec-path-from-shell-variables var))
;;   (exec-path-from-shell-initialize))


(provide 'init-emacs)
