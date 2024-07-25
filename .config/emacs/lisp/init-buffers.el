;; init-buffers.el - buffer and window management  -*- lexical-binding: t; -*-

(savehist-mode)				; save minibuffer history
(setq
 savehist-file (no-littering-expand-var-file-name "savehist.el")
 savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(desktop-save-mode)			; save frame configuration

(general-def				; buffer keybinds
  "M-0"       'delete-window
  "M-1"       'delete-other-windows
  "M-2"       'split-window-below
  "M-3"       'split-window-right
  "M-o"       (lambda () (interactive) (other-window +1))
  "M-i"       (lambda () (interactive) (other-window -1))
  "C-c M-i"   'tab-to-tab-stop		; former M-i
  "M-k"       (lambda () (interactive) (kill-buffer nil))
  "C-M-k"     'kill-sexp
  "C-c M-k"   'kill-sentence
  "C-x C-M-k" 'kill-other-buffers)

(use-package bufler			; a butler for your buffers
  :general ("C-x b" 'bufler))

;; minibuffer
(use-package vertico                    ; VERTical Interactive COmpletion
  :custom
  (vertico-count 17)
  (vertico-cycle t)
  (vertico-sort-function 'vertico-sort-history-alpha)
  :config
  (vertico-mode)
  (vertico-mouse-mode))

(use-package orderless			; completion style
  :custom (completion-styles '(orderless basic)))

(use-package consult			; navigation
  :general
  ("C-x C-b"     'consult-buffer
   "C-x p b"   'consult-project-buffer
   "C-o"       'consult-imenu
   ;; "C-h a"     'describe-symbol
   "M-y"       'consult-yank-pop
   "C-M-s"     'consult-ripgrep
   "C-x M-f"   'consult-fd
   [remap goto-line] 'consult-goto-line
   "C-x M-:"   'consult-complex-command)
  (:keymaps 'isearch-mode-map
            "C-o" 'consult-line)

  (consult-customize consult-theme :preview-key '(:debounce 0.5 any)))

(provide 'init-buffers)
