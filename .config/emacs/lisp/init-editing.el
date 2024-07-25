;; init-editing.el - Emacs config for editing  -*- lexical-binding: t; -*-

(global-auto-revert-mode)

(general-def
  "M-z"   'toggle-truncate-lines
  "C-M-z" 'zap-up-to-char)

(use-package mwim                       ; better C-a/C-e
  :general
  ([remap move-beginning-of-line] 'mwim-beginning-of-code-or-line
   [remap move-end-of-line]       'mwim-end-of-code-or-line))

(use-package expand-region		; expand selection
  :general ("C-=" 'er/expand-region))

(use-package deadgrep)                  ; ripgrep UI

(use-package visual-regexp
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
  ;; (undo-tree-history-directory-alist backup-directory-alist)
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  :config
  (global-undo-tree-mode))

(use-package ws-butler                  ; delete trailing whitespace
  :diminish
  :custom (ws-butler-keep-whitespace-before-point nil)
  :config (ws-butler-global-mode))

(use-package corfu			; completions
  :init (global-corfu-mode) (corfu-popupinfo-mode)
  :custom
  (tab-always-indent 'complete)
  (corfu-auto t))

;; TODO: make work
;; (use-package copilot                    ; GitHub Copilot
;;   :ensure (:host github :repo "copilot-emacs/copilot.el")
;;   :hook (prog-mode . copilot-mode)
;;         )

;; TODO: cape?


(provide 'init-editing)
