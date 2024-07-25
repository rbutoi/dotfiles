;; init-buffers.el - Buffer and window management  -*- lexical-binding: t; -*-

(recentf-mode)                          ; recent files
(save-place-mode)                       ; remember buffer location
(savehist-mode)				; save minibuffer history
(setopt
 savehist-file (no-littering-expand-var-file-name "savehist.el")
 savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

;; TODO: tricky to get initialization right
;; (add-hook 'elpaca-after-init-hook 'desktop-save-mode)

;;;;;;;;;;;;;;
;; keybinds ;;
;;;;;;;;;;;;;;

(general-def
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
  "C-x M-k"   'kill-other-buffers)

;;;;;;;;;;;;;;;;;;;;;;
;; extenal packages ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package bufler			; a butler for your buffers
  :general ("C-x b" 'bufler))

;; minibuffer
(use-package vertico                    ; VERTical Interactive COmpletion
  :demand t                             ; otherwise first invocation is a dud
  :hook ((minibuffer-setup . vertico-repeat-save))
  :general ("C-x C-r" 'vertico-repeat)
  :custom
  (vertico-count 17)
  (vertico-cycle t)
  (vertico-sort-function 'vertico-sort-history-alpha)
  :config
  (vertico-mode)
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

(use-package marginalia                 ; extra info in margins
  :config (marginalia-mode))

(use-package orderless			; completion style
  :custom (completion-styles '(orderless basic)))

(use-package consult			; navigation
  :general                              ; remap some standard commands
  ("C-x C-b"   'consult-buffer
   "C-x p b"   'consult-project-buffer
   "C-o"       'consult-imenu
   ;; "C-h a"     'describe-symbol TODO
   "M-y"       'consult-yank-pop
   "C-M-s"     'consult-ripgrep
   "C-x f"     'consult-fd
   "C-x M-f"   'set-fill-column
   [remap goto-line] 'consult-goto-line
   "C-x M-:"   'consult-complex-command)
  (:keymaps 'isearch-mode-map
            "C-o" 'consult-line)
  :config
  ;; From upstream docs:
  ;; https://github.com/minad/consult/blob/1.8/README.org?plain=1#L999-L1009
  (consult-customize
   consult-line
   :add-history (seq-some #'thing-at-point '(region symbol)))
  (defalias 'consult-line-thing-at-point 'consult-line)
  (consult-customize
   consult-line-thing-at-point
   :initial (thing-at-point 'symbol))
  (general-def "C-M-o" 'consult-line-thing-at-point)

  (consult-customize consult-theme :preview-key '(:debounce 0.5 any)))


(provide 'init-buffers)
