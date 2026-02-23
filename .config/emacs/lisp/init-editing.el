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
  "s-r"   'revert-buffer-quick
  "M-z"   'toggle-truncate-lines
  "C-M-z" 'visual-line-mode
  "M-Z"   'zap-up-to-char
  "s-v"   'clipboard-yank)

(use-package mwim                       ; smarter basics
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

(use-package vundo                      ; visual undo
  :general ("C-z"   'vundo
            "C-x u" 'vundo))
(use-package goto-chg                   ; goto last edit position
  :general ("C-," 'goto-last-change))

(use-package treemacs                   ; side panel file tree
  :defer 1
  :general
  (  "C-S-M-SPC" 'treemacs)
  ("C-x C-M-SPC" 'treemacs)             ; cli
  :custom
  (treemacs-eldoc-display 'detailed)
  (treemacs--project-follow-delay 0.3)
  :config
  (treemacs-project-follow-mode))
(use-package treemacs-magit :after treemacs)

(use-package smartparens
  :general ("M-D" 'sp-splice-sexp)
  :config (require 'smartparens-config)) ; don't enable major mode

(use-package flyspell-correct
  :after flyspell
  :general ([remap ispell-word] 'flyspell-correct-wrapper))

(use-package expand-region              ; expand selection
  :general ("C-=" 'er/expand-region))

(use-package multiple-cursors)          ; edit like it's 1999

(use-package deadgrep)                  ; ripgrep UI

(use-package visual-regexp              ; visual replace
  :general
  ([remap query-replace-regexp] 'vr/query-replace)
  ([remap query-replace]        'vr/query-replace))

(use-package pcre2el :hook (elpaca-after-init . pcre-mode))
(use-package ialign                     ; interactive align regexp
  :general ("C-x l" 'ialign))           

(use-package ws-butler                  ; automatically trim whitespace
  :hook   (elpaca-after-init . ws-butler-global-mode)
  :custom (ws-butler-keep-whitespace-before-point nil))

(use-package hungry-delete              ; delete consecutive whitespace
  :hook   (elpaca-after-init . global-hungry-delete-mode)
  :custom (hungry-delete-join-reluctantly t)) ; leave a space between words

(use-package fancy-fill-paragraph       ; better fill-paragraph
  :general ([remap fill-paragraph] 'fancy-fill-paragraph)
  :config
  (add-to-list 'fancy-fill-paragraph-dot-point-prefix "* "))

(use-package so-long-mode :ensure nil   ; huge file support
  :hook (elpaca-after-init . global-so-long-mode))

(use-package visual-fill-column)        ; wrap lines at fill-column


(provide 'init-editing)
