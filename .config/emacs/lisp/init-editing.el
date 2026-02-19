;;; init-editing.el ---  Editing  -*- lexical-binding: t; -*-

(general-add-hook 'elpaca-after-init-hook
                  '(delete-selection-mode ; typing overwrites selection
                    electric-pair-mode    ; automatic ()
                    global-auto-revert-mode))
(with-eval-after-load 'elec-pair
  (add-to-list 'electric-pair-pairs '( ?\` . ?\`)))

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
  :hook (visual-line-mode . visual-fill-column-for-vline))

(use-package undo-tree                  ; visual undo
  :hook    (elpaca-after-init . global-undo-tree-mode)
  :general ("C-z" 'undo-tree-undo)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t))

;; TODO or: https://github.com/purcell/whole-line-or-region
(use-package mwim
  :general
  ([remap move-end-of-line]       'mwim-end-of-code-or-line))
(use-package crux :demand t
  :custom (kill-whole-line t)
  :general
  ([remap move-beginning-of-line] 'crux-move-beginning-of-line
   "C-k"      'crux-smart-kill-line
   "C-x M-k"  'crux-kill-other-buffers
   "C-c d"    'crux-duplicate-current-line-or-region
   "C-c M-d"  'crux-duplicate-and-comment-current-line-or-region
   "M-o"      'crux-other-window-or-switch-buffer
   "C-c I"    'crux-find-user-init-file
   "C-c S"    'crux-find-shell-init-file
   "C-^"      'crux-top-join-line
   "C-x C-u"  'crux-upcase-region
   "C-x C-l"  'crux-downcase-region
   "C-g"      'crux-keyboard-quit-dwim)
  :config
  (crux-with-region-or-sexp-or-line kill-region)
  (crux-with-region-or-line         kill-ring-save))

(use-package goto-chg
  :general
  ("M-m" 'goto-last-change)
  ("M-M" 'goto-last-change-reverse))

(use-package smartparens
  :general ("M-D" 'sp-splice-sexp)
  :config (require 'smartparens-config)) ; don't enable major mode

(use-package flyspell-correct
  :after flyspell
  :general ([remap ispell-word] 'flyspell-correct-wrapper))

(use-package expand-region              ; expand selection
  :general ("C-=" 'er/expand-region))

(use-package rg                         ; ripgrep UI
  :after transient
  :config (add-to-list 'rg-command-line-flags "--multiline"))

(use-package visual-regexp              ; visual replace
  :general
  ([remap query-replace-regexp] 'vr/query-replace)
  ([remap query-replace]        'vr/query-replace))

(use-package pcre2el
  :hook (elpaca-after-init . pcre-mode))
(use-package ialign
  :general ("C-x l" 'ialign))           ; interactive align regexp

(use-package ws-butler                 ; automatically trim whitespace
  :hook   elpaca-after-init
  :custom (ws-butler-keep-whitespace-before-point nil))

(use-package hungry-delete              ; delete consecutive whitespace
  :hook   (elpaca-after-init . global-hungry-delete-mode)
  :custom (hungry-delete-join-reluctantly t)) ; leave a space between words

(use-package fancy-fill-paragraph
  :general ([remap fill-paragraph] 'fancy-fill-paragraph)
  :config
  (add-to-list 'fancy-fill-paragraph-dot-point-prefix "* "))


(provide 'init-editing)
