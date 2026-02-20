;;; init-completion.el --- Completion framework  -*- lexical-binding: t; -*-

;; Minibuffer
(use-package vertico                    ; VERTical Interactive COmpletion
  :hook (elpaca-after-init
         (minibuffer-setup . minibuffer-depth-indicate-mode)
         (minibuffer-setup . vertico-repeat-save)
         (vertico-mode . vertico-multiform-mode))
  :general
  ("C-x C-r" 'vertico-repeat)
  (:keymaps 'vertico-map
            "<escape>" #'minibuffer-keyboard-quit
            "C-M-n" #'vertico-next-group
            "C-M-p" #'vertico-previous-group)
  :custom
  (vertico-count 17)
  (vertico-cycle t)
  (vertico-sort-function 'vertico-sort-history-alpha)
  :config
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'vertico-repeat-history)))

(use-package vertico-posframe
  :demand t
  :after vertico
  :config
  (setopt vertico-multiform-commands
          '((consult-line-multi          (:not posframe))
            (consult-line                (:not posframe))
            (consult-line-thing-at-point (:not posframe))
            (consult-imenu               (:not posframe))
            (consult-ripgrep             (:not posframe))
            (t                                 posframe))))

(use-package marginalia :hook elpaca-after-init)

(use-package orderless :custom (completion-styles '(orderless basic)))

(use-package consult                    
  :general                              
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
  (:keymaps
   'consult-narrow-map
   "C-M-h" 'consult-narrow-help)
  (:keymaps
   'isearch-mode-map
   "C-o"       'consult-line
   "M-s o"     'consult-line-multi)
  (:keymaps
   'minibuffer-local-map
   [remap next-matching-history-element]     'consult-history
   [remap previous-matching-history-element] 'consult-history)

  :custom
  (xref-search-program 'ripgrep)
  (xref-show-definitions-function #'consult-xref)
  (xref-show-xrefs-function #'consult-xref)
  (consult-fd-args '((if (executable-find "fdfind" 'remote) "fdfind" "fd")
                     "--full-path --color=never --hidden"))
  :config
  (defalias 'consult-line-thing-at-point 'consult-line)
  (consult-customize
   consult-line-thing-at-point
   :initial (thing-at-point 'symbol))
  (consult-customize
   consult-theme consult-buffer consult-ripgrep
   :preview-key '(:debounce 0.1 any)))  ; TODO: consult-narrow-*
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
  :after grep
  :general
  (:keymaps 'grep-mode-map
            "e"       'wgrep-change-to-wgrep-mode   ; occur-style
            "C-x C-q" 'wgrep-change-to-wgrep-mode)) ; dired-style

;; Inline completions
(use-package corfu
  :hook ((elpaca-after-init . global-corfu-mode)
         (elpaca-after-init . global-completion-preview-mode)
         (global-corfu-mode . corfu-popupinfo-mode))
  :custom
  (corfu-auto t))
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))


(provide 'init-completion)
