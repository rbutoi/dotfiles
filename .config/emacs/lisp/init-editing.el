;; init-editing.el -  Editing  -*- lexical-binding: t; -*-

(delete-selection-mode)                 ; typing overwrites selection
(electric-pair-mode)                    ; automatic ()
(add-to-list 'electric-pair-pairs '( ?\` . ?\`))

(global-auto-revert-mode)
(setopt global-auto-revert-non-file-buffers t
        set-mark-command-repeat-pop t ; can keep C-u C-SPC C-SPC C-SPC...
        kill-do-not-save-duplicates t
        truncate-lines t
        fill-column 80)

;;;;;;;;;;;;;;
;; keybinds ;;
;;;;;;;;;;;;;;

(general-def
  "M-z"   'toggle-truncate-lines
  "C-M-z" 'zap-up-to-char
  "s-v"   'clipboard-yank)

;;;;;;;;;;;;;;;;;;;;;;
;; extenal packages ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package jinx
  :diminish
  :hook (elpaca-after-init-hook . global-jinx-mode)
  :bind (("M-$" . jinx-correct)))

(use-package mwim                       ; better C-a/C-e
  :general
  ([remap move-beginning-of-line] 'mwim-beginning-of-code-or-line
   [remap move-end-of-line]       'mwim-end-of-code-or-line))

(use-package expand-region              ; expand selection
  :general ("C-=" 'er/expand-region))

(use-package rg                         ; ripgrep UI
  :config (add-to-list 'rg-command-line-flags "--multiline"))

(use-package visual-regexp              ; visual replace
  :general
  ([remap query-replace-regexp] 'vr/query-replace)
  ([remap query-replace] 'vr/query-replace))

(use-package ialign
  :general ("C-x l" 'ialign))           ; interactive align regexp

(use-package hungry-delete              ; delete consecutive whitespace
  :custom (hungry-delete-join-reluctantly t) ; leave a space between words
  ;; :general ("C-c h" 'hungry-delete-mode) TODO: maybe??
  :config (global-hungry-delete-mode))

(use-package undo-tree                  ; visual undo
  :defer 2
  :diminish
  :general
  ("C-z"   'undo-tree-undo)
  :custom
  (undo-tree-history-directory-alist backup-directory-alist)
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  :config
  (global-undo-tree-mode))

(use-package so-long :config (global-so-long-mode)) ; long file handling

(use-package ws-butler                 ; automatically trim whitespace
  :diminish
  :custom (ws-butler-keep-whitespace-before-point nil)
  :config (ws-butler-global-mode))

(use-package smartparens
  :general ("M-D" 'sp-splice-sexp)
  :config
  (require 'smartparens-config))        ; don't enable major mode


(provide 'init-editing)
