;; init-buffers.el - Buffer and window management  -*- lexical-binding: t; -*-

(recentf-mode)                          ; recent files
(save-place-mode)                       ; remember buffer location
(savehist-mode)                         ; save minibuffer history
(setopt
 savehist-file (no-littering-expand-var-file-name "savehist.el")
 savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
 enable-recursive-minibuffers t)

;;;;;;;;;;;;;;
;; keybinds ;;
;;;;;;;;;;;;;;

(general-def
  "M-0"       'delete-window
  "M-1"       'delete-other-windows
  "M-2"       'split-window-below
  "M-3"       'split-window-right
  "M-o"       (cmd! (other-window +1))
  "M-i"       (cmd! (other-window -1))
  "C-c M-i"   'tab-to-tab-stop		; former M-i
  "M-k"       (cmd! (kill-buffer nil))
  "C-M-k"     'kill-sexp
  "C-c M-k"   'kill-sentence
  "C-x M-k"   'kill-other-buffers
  "s-n"       'make-frame-command
  "s-w"       'delete-frame)

;;;;;;;;;;;;;;;;;;;;;;
;; extenal packages ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package bufler			; a butler for your buffers
  :general ("C-x b" 'bufler))

;; minibuffer
(use-package recursion-indicator :config (recursion-indicator-mode))

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
   "C-x M-:"   'consult-complex-command
   "C-M-o"     'consult-line-thing-at-point
   "M-s o"     'consult-line-multi
   "M-s M-o"   'consult-line-multi)
  (:keymaps 'isearch-mode-map
            "C-o"   'consult-line
            "M-s o" 'consult-line-multi)
  :custom
  (consult-fd-args '((if (executable-find "fdfind" 'remote) "fdfind" "fd")
                     "--full-path --color=never --hidden"))
  :config
  (defalias 'consult-line-thing-at-point 'consult-line)
  (consult-customize
   consult-line-thing-at-point
   :initial (thing-at-point 'symbol))
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)))

(use-package consult-dir
  :after vertico
  :general
  ("C-x C-d" 'consult-dir)
  (:keymaps 'vertico-map
            "C-x C-d" 'consult-dir
            "C-x C-j" 'consult-dir-jump-file))
(use-package embark
  :general
  ("C-." 'embark-act)
  (:keymaps 'minibuffer-local-map
            "C-c C-c" 'embark-collect
            "C-c C-e" 'embark-export))
(use-package embark-consult)
(use-package wgrep
  :init (require 'grep)
  :general
  (:keymaps 'grep-mode-map
            "e"       'wgrep-change-to-wgrep-mode   ; occur-style
            "C-x C-q" 'wgrep-change-to-wgrep-mode)) ; dired-style


(provide 'init-buffers)
