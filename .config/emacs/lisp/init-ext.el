;;; init-ext.el --- External integrations  -*- lexical-binding: t; -*-

(xterm-mouse-mode)                      ; mouse in terminal Emacs
(setopt dired-mouse-drag-files t)

(use-package man                        ; man(1)
  :ensure nil
  :custom
  (Man-width-max nil)
  (Man-notify-method 'aggressive)
  :general (:keymaps 'Man-mode-map "/" 'isearch-forward-word))

;; Dired
(use-package dired-hide-dotfiles        ; file manager
  :general (:keymaps 'dired-mode-map "." 'dired-hide-dotfiles-mode))
(use-package dired-gitignore
  :init (dired-gitignore-global-mode t)
  :general (:keymaps 'dired-mode-map "M-." 'dired-gitignore-global-mode))

;; Terminal
(use-package vterm                      ; terminal
  :defer 1
  :custom
  (vterm-always-compile-module t))
(use-package vterm-toggle
  :after vterm
  :general
  ("<f5>" 'vterm-toggle)
  (:keymaps 'vterm-mode-map "<f5>" 'vterm-toggle))

(use-package google-this)               ; Google word at point

(use-package atomic-chrome              ; edit Chrome text fields in Emacs
  :config
  (atomic-chrome-start-server))

;; OS-specific
(with-system darwin
  (setopt mac-option-modifier       'meta
          mac-command-modifier      'super
          insert-directory-program  "gls" ; gnu coreutils
          manual-program            "gman"))


(provide 'init-ext)
