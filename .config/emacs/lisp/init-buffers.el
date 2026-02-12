;;; init-buffers.el --- Buffer and window management  -*- lexical-binding: t; -*-

(recentf-mode)                          ; recent files
(save-place-mode)                       ; remember buffer location
(savehist-mode)                         ; save minibuffer history
(dolist (v '(kill-ring
             mark-ring
             search-ring
             regexp-search-ring))
  (add-to-list 'savehist-additional-variables v))
(setopt recentf-max-saved-items 100
        enable-recursive-minibuffers t)

(general-def :keymaps 'override
  "M-0"       'delete-window
  "M-1"       'delete-other-windows
  "M-2"       'split-window-below
  "M-3"       'split-window-right
  "M-o"       'other-window
  "M-i"       (cmd! (other-window -1)))
(general-def
  "C-c M-i"   'tab-to-tab-stop          ; former M-i
  "M-k"       'kill-current-buffer
  "C-M-k"     'kill-sexp
  "C-c M-k"   'kill-sentence
  "C-x M-k"   'kill-other-buffers
  "s-n"       'make-frame-command
  "s-w"       'delete-frame)

(use-package bufler			; a butler for your buffers
  :general ("C-x b" 'bufler))


(provide 'init-buffers)
