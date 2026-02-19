;;; init-ext.el --- External integrations  -*- lexical-binding: t; -*-

;; (add-hook 'elpaca-after-init-hook #'xterm-mouse-mode) ; mouse in terminal Emacs
(setopt dired-mouse-drag-files t)

(use-package man :ensure nil
  :custom
  (Man-width-max nil)
  (Man-notify-method 'aggressive)
  :general (:keymaps 'Man-mode-map "/" 'isearch-forward-word))

(use-package dired-hide-dotfiles        
  :general (:keymaps 'dired-mode-map "." 'dired-hide-dotfiles-mode))
(use-package dired-gitignore
  :hook (dired-load . dired-gitignore-global-mode)
  :general (:keymaps 'dired-mode-map "M-." 'dired-gitignore-global-mode))

(use-package vterm                      
  :custom (vterm-always-compile-module t))
(use-package vterm-toggle
  :general ("<f5>" 'vterm-toggle)
  (:keymaps 'vterm-mode-map
            "<f5>" 'vterm-toggle))

(use-package google-this)               ; Google word at point

(use-package atomic-chrome              ; edit Chrome text fields in Emacs
  :disabled                             ; hmm
  :config
  (atomic-chrome-start-server))


(provide 'init-ext)
