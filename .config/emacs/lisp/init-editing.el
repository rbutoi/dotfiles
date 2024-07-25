;; init-editing.el -  Editing  -*- lexical-binding: t; -*-

(delete-selection-mode)                 ; typing overwrites selection
(electric-pair-mode)                    ; automatic ()
(global-auto-revert-mode)
(setopt global-auto-revert-non-file-buffers   t
        set-mark-command-repeat-pop           t ; can keep C-u C-SPC C-SPC C-SPC...
        kill-do-not-save-duplicates           t)

(use-package flyspell                   ; spellcheck
  :ensure nil
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :general (:keymaps                    ; default binds are a little overzealous
            'flyspell-mode-map "C-," nil "C-." nil "C-;" nil "C-M-i" nil))

;;;;;;;;;;;;;;
;; keybinds ;;
;;;;;;;;;;;;;;

(general-def
  "M-z"   'toggle-truncate-lines
  "C-M-z" 'zap-up-to-char)

;;;;;;;;;;;;;;;;;;;;;;
;; extenal packages ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package mwim                       ; better C-a/C-e
  :general
  ([remap move-beginning-of-line] 'mwim-beginning-of-code-or-line
   [remap move-end-of-line]       'mwim-end-of-code-or-line))

(use-package expand-region		; expand selection
  :general ("C-=" 'er/expand-region))

(use-package deadgrep)                  ; ripgrep UI

(use-package visual-regexp              ; visual replace
  :general
  ([remap query-replace-regexp] 'vr/query-replace)
  ([remap query-replace] 'vr/query-replace))

(use-package ialign
  :general ("C-x l" 'ialign))           ; interactive align regexp

(use-package undo-tree                  ; visual undo
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

;; TODO: or just electric-pair-mode? what is a replacement for sp-splice-sexp?

;; (use-package smartparens                ; parentheses
;;   :general ("M-D" 'sp-splice-sexp)
;;   :config
;;   (require 'smartparens-config)
;;   (smartparens-global-mode))


(provide 'init-editing)
