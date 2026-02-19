;;; init-buffers.el --- Buffer and window management  -*- lexical-binding: t; -*-

(setopt enable-recursive-minibuffers t)

(use-package recentf    :ensure nil :hook elpaca-after-init
  :custom
  (find-file-visit-truename  t) ; resolve symlinks so recentf doesn't keep both names
  (recentf-max-saved-items   100)
  (recentf-auto-cleanup      nil)       ; TODO, not at start though
  :config
  (add-to-list 'recentf-exclude (locate-user-emacs-file "elpaca/")))
(use-package save-place :ensure nil :hook elpaca-after-init)
(use-package savehist   :ensure nil :hook elpaca-after-init
  :config
  (dolist (v '(kill-ring
               mark-ring
               search-ring
               regexp-search-ring))
    (add-to-list 'savehist-additional-variables v)))

(general-def :keymaps 'override
  "M-0"       'delete-window
  "M-1"       'delete-other-windows
  "M-2"       'split-window-below
  "M-3"       'split-window-right
  "M-i"       (cmd! (other-window -1)))
(general-def
  "C-c M-i"   'tab-to-tab-stop          ; former M-i
  ;; still considering
  ;; "M-k"       'kill-current-buffer
  "s-n"       'make-frame-command
  "s-w"       'delete-frame)

(use-package bufler			; a butler for your buffers
  :general ("C-x b" 'bufler))


(provide 'init-buffers)
