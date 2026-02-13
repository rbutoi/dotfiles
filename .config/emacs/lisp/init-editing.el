;;; init-editing.el ---  Editing  -*- lexical-binding: t; -*-

(delete-selection-mode)                 ; typing overwrites selection
(electric-pair-mode)                    ; automatic ()
(add-to-list 'electric-pair-pairs '( ?\` . ?\`))

(global-auto-revert-mode)
(setopt global-auto-revert-non-file-buffers t
        set-mark-command-repeat-pop         t ; can keep C-u C-SPC C-SPC C-SPC...
        kill-do-not-save-duplicates         t
        truncate-lines                      t
        fill-column                         80
        ediff-window-setup-function         'ediff-setup-windows-plain)

(general-def
  "C-S-k" 'kill-whole-line
  "M-z"   'toggle-truncate-lines
  "C-M-z" 'visual-line-mode
  "M-Z"   'zap-up-to-char
  "s-v"   'clipboard-yank)
(use-package visual-fill-column
  :config
  (add-hook 'visual-line-mode-hook #'visual-fill-column-for-vline))

(use-package undo-tree                  ; visual undo
  :defer 2
  :general
  ("C-z"   'undo-tree-undo)
  :custom
  (undo-tree-history-directory-alist backup-directory-alist)
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  :config
  (global-undo-tree-mode))

;; TODO or: https://github.com/purcell/whole-line-or-region
(use-package mwim
  :general
  ([remap move-end-of-line]       'mwim-end-of-code-or-line))
(use-package crux
  :general
  ([remap move-beginning-of-line] 'crux-move-beginning-of-line
   "C-k"      'crux-smart-kill-line
   "C-x M-k"  'crux-kill-other-buffers
   "C-c d"    'crux-duplicate-current-line-or-region
   "C-c M-d"  'crux-duplicate-and-comment-current-line-or-region
   "M-o"      'crux-other-window-or-switch-buffer
   "C-c i"    'crux-find-user-init-file
   "C-c s"    'crux-find-shell-init-file
   "C-^"      'crux-top-join-line
   "C-x C-u"  'crux-upcase-region
   "C-x C-l"  'crux-downcase-region
   "C-g"      'crux-keyboard-quit-dwim
   )
  :config
  (crux-with-region-or-sexp-or-line kill-region)
  (crux-with-region-or-line         kill-ring-save))

(use-package goto-chg
  :general
  ("M-m" 'goto-last-change)
  ("M-M" 'goto-last-change-reverse))

(use-package smartparens
  :general ("M-D" 'sp-splice-sexp)
  :config
  (require 'smartparens-config))        ; don't enable major mode

(use-package flyspell-correct
  :after flyspell
  :general
  ([remap ispell-word] 'flyspell-correct-wrapper))

(use-package expand-region              ; expand selection
  :general ("C-=" 'er/expand-region))

(use-package rg                         ; ripgrep UI
  :config (add-to-list 'rg-command-line-flags "--multiline"))

(use-package visual-regexp              ; visual replace
  :general
  ([remap query-replace-regexp] 'vr/query-replace)
  ([remap query-replace]        'vr/query-replace))

(use-package pcre2el
  :config
  (pcre-mode))
(use-package ialign
  :general ("C-x l" 'ialign))           ; interactive align regexp

(use-package ws-butler                 ; automatically trim whitespace
  :custom (ws-butler-keep-whitespace-before-point nil)
  :config (ws-butler-global-mode))

(use-package hungry-delete              ; delete consecutive whitespace
  :custom (hungry-delete-join-reluctantly t) ; leave a space between words
  :config (global-hungry-delete-mode))


(provide 'init-editing)
