;; -*- lexical-binding: t; -*-
;; init.el - Emacs config

;;;; Prologue
;; package management: straight.el / use-package
(setq
 straight-use-package-by-default t
 straight-base-dir  (concat user-emacs-directory "var/") ; no f-join yet
 straight-build-dir                ; allow multiple emacsen to coexist
 (concat "build-" emacs-version))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "var/straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package benchmark-init             ; start benchmark
  :hook (after-init . (lambda ()
                        (message "Emacs loaded in %s" (emacs-init-time))
                        (benchmark-init/deactivate))))

(use-package gcmh :init (gcmh-mode)) ; GC magic hack: gitlab.com/koral/gcmh

(use-package f)                         ; add local load path
(add-to-list 'load-path (f-join user-emacs-directory "lisp/"))
(load "config-fns.el")                  ; useful function definitions

(use-package s)         ; host identification. (system-name) is lacking the "-f"
(let ((host (s-trim (shell-command-to-string "hostname -f"))))
  (defconst my/crostini?    (not (not (string-match-p "penguin" host))))
  (defconst my/laptop?      (not (not (string-match-p    "roam" host))))
  (defconst my/wayland?     (not (not (getenv "WAYLAND_DISPLAY"))))
  (defconst my/work?        (not (not (string-match-p  "\.com$" host))))
  (defconst my/workstation? (and my/work? (not my/laptop?))))

;;;; UI / UX
(tool-bar-mode -1) (menu-bar-mode -1) ; just use F10
(context-menu-mode)                   ; this is good tho
(global-hl-line-mode)                 ; always good to keep track

(defun my/terminal-bg-transparency ()
  "Disable background in terminal to show wallpaper."
  (unless (display-graphic-p) (set-face-background 'default "unspecified-bg")))
(use-package doom-themes
  :init (load-theme 'doom-gruvbox t)
  (my/terminal-bg-transparency)
  :hook ((server-after-make-frame . my/terminal-bg-transparency)))

(use-package general)                   ; keybinds
(general-def
  "C-x C-m"   'execute-extended-command ; more convenient than M-x
  "C-x C-M-c" 'save-buffers-kill-emacs
  "M-0"       'delete-window            ; buffers and windows
  "C-x k"     'my/kill-this-buffer
  "C-x M-k"   'my/kill-all-buffers
  "C-x K"     'kill-buffer)
(general-def :keymaps '(global magit-mode-map) ; just drop the M- in magit
  "M-1"       'delete-other-windows
  "M-2"       'split-window-below
  "M-3"       'split-window-right
  "M-o"       (lambda () (interactive) (other-window +1))
  "M-i"       (lambda () (interactive) (other-window -1)))

(use-package which-key                  ; useful shortcut reminders
  :init (which-key-setup-side-window-bottom) (which-key-mode)
  :custom (which-key-idle-secondary-delay 0.01))

(use-package ace-window
  :general ([remap other-window] 'ace-window)
  :custom (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
(use-package avy
  :init (avy-setup-default)
  :general
  ("C-c C-k" 'avy-resume)
  ("M-j" 'avy-goto-char-timer))

(use-package doom-modeline              ; modeline
  :init
  (setq doom-modeline-support-imenu t
        doom-modeline-enable-word-count t
        doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (column-number-mode)
  (doom-modeline-mode)
  :hook
  ((after-change-major-mode . doom-modeline-conditional-buffer-encoding)))
(use-package all-the-icons)

(use-package helpful                    ; improved help windows
  :general
  ([remap describe-command]  'helpful-command)
  ([remap describe-function] 'helpful-function)
  ([remap describe-key]      'helpful-key)
  ([remap describe-symbol]   'helpful-symbol)
  ([remap describe-variable] 'helpful-variable))

(use-package popper                     ; popups
  :general
  ("M-`"   'popper-toggle-latest
   ;; TODO: find terminal friendly binds
   ;; "C-`"   'popper-cycle
   ;; "C-M-`" 'popper-toggle-type
   )
  :custom (popper-reference-buffers
           '("\\*Messages\\*"
             "Output\\*$"
             "\\*Async Shell Command\\*"
             help-mode
             helpful-mode))
  :init (popper-mode) (popper-echo-mode)) ; For echo area hints

;; completion
(setq tab-always-indent 'complete
      ;; TAB cycle if there are only few candidates
      completion-cycle-threshold 3)
(use-package corfu
  :init (global-corfu-mode)
  :custom (corfu-auto t)
  :general (:keymaps 'corfu-map "M-m" 'corfu-move-to-minibuffer)
  :config
  (use-package corfu-doc
    :hook corfu-mode
    :general (:keymaps 'corfu-map
                       "M-d" 'corfu-doc-toggle
                       "M-p" 'corfu-doc-scroll-down
                       "M-n" 'corfu-doc-scroll-up)))
(use-package corfu-terminal :after corfu
  :straight '(corfu-terminal
              :type git
              :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :config (unless (display-graphic-p) (corfu-terminal-mode)))
(use-package corfu-doc-terminal :after corfu-doc
  :straight '(corfu-doc-terminal
              :type git
              :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git")
  :config (unless (display-graphic-p) (corfu-doc-terminal-mode)))

(use-package vertico                    ; VERTical Interactive COmpletion
  :custom
  (vertico-count 17)
  (vertico-cycle t)
  :config (vertico-mode))

(use-package consult
  :init
  (defun my/consult-fd-config ()
    "consult-fd on ~/.config"
    (interactive) (consult-fd "~/.config/" ""))
  (defun my/consult-line-symbol-at-point ()
    "consult-line the symbol at point."
    (interactive) (consult-line (thing-at-point 'symbol)))
  :general                              ; remap some standard commands
  ("C-x M-:" 'consult-complex-command
   "C-x C-b" 'consult-buffer
   "C-x b"   'consult-recent-file
   "C-x p b" 'consult-project-buffer
   "C-M-o"   'my/consult-line-symbol-at-point
   "C-o"     'consult-imenu
   "C-h a"   'consult-apropos
   "M-y"     'consult-yank-pop
   "C-M-s"   'consult-ripgrep
   "C-x M-f" 'consult-fd
   "C-c M-c" 'my/consult-fd-config)
  (:keymaps 'isearch-mode-map
            "C-o" 'consult-line)
  :custom
  (xref-show-xrefs-function       #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config (recentf-mode))

(use-package savehist                   ; save minibuffer history
  :straight (:type built-in)
  :after no-littering
  :custom
  (savehist-file (no-littering-expand-var-file-name "savehist.el"))
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  :config (savehist-mode))

(use-package marginalia :config (marginalia-mode))

(use-package orderless
  :custom (completion-styles '(orderless basic)))

(use-package embark
  :general
  ("C-." 'embark-act
   "C-;" 'embark-dwim
   "C-h B" 'embark-bindings)
  :custom
  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;;; Editing
(setq set-mark-command-repeat-pop t) ; can keep C-u C-SPC C-SPC C-SPC...

(delete-selection-mode)                 ; typing overwrites selection
(use-package sudo-edit)
(use-package so-long :init (global-so-long-mode)) ; long file handling

(use-package mwim                       ; better C-a/C-e
  :general
  ([remap move-beginning-of-line] 'mwim-beginning-of-code-or-line
   [remap move-end-of-line]       'mwim-end-of-code-or-line))

(use-package undo-tree                  ; visual undo
  :defer 2
  :general
  ("C-z"   'undo-tree-undo
   "C-x u" 'undo-tree-visualize)
  :custom
  (undo-tree-history-directory-alist backup-directory-alist)
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  :config (global-undo-tree-mode))

(use-package expand-region
  :general ("M-=" 'er/expand-region))   ; terminal-friendly bind

(use-package deadgrep)                  ; ripgrep UI

(use-package ws-butler                  ; delete trailing whitespace
  :custom (ws-butler-keep-whitespace-before-point nil)
  :config (ws-butler-global-mode))
(use-package column-enforce-mode        ; highlight text past fill column
  ;; TODO: find a shortcut. globally it's a little annoying
  ;; :init (global-column-enforce-mode)
  )

(use-package aggressive-indent          ; keep indented
  :hook ((emacs-lisp-mode . aggressive-indent-mode))
  :custom (aggressive-indent-sit-for-time 0.5)) ; slow it down

(use-package flyspell                   ; spellcheck
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))
;; (use-package flyspell-correct
;;   :after flyspell
;;   :general
;;   (:keymaps 'flyspell-mode-map
;;             "C-;" 'flyspell-correct-wrapper))

;;;; Programming
(setq-default indent-tabs-mode nil)     ; never tabs to indent
(setq vc-follow-symlinks t              ; don't prompt
      vc-make-backup-files t
      comment-auto-fill-only-comments t)
(toggle-text-mode-auto-fill)
(add-hook 'prog-mode-hook 'auto-fill-mode)

(general-add-hook '(text-mode-hook prog-mode-hook) 'display-line-numbers-mode)

(use-package tree-sitter
  :defer 3
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs :after tree-sitter)

(use-package yasnippet
  :defer 4
  :hook ((prog-mode) . yas-minor-mode)
  :config
  (use-package yasnippet-snippets)
  (yas-reload-all))

(use-package smartparens                ; parentheses
  :defer 1
  :init (require 'smartparens-config) (smartparens-global-mode))
(use-package rainbow-delimiters :hook prog-mode)
(use-package rainbow-mode       :hook prog-mode)

(use-package string-inflection        ; toggle underscore -> UPCASE -> CamelCase
  :general (:keymaps '(prog-mode-map c-mode-base-map sh-mode-map)
                     "C-c C-u" 'string-inflection-cycle))

(use-package auto-highlight-symbol      ; highlight symbols
  :init (global-auto-highlight-symbol-mode))
(use-package hl-todo                   ; highlight "TODO:"s
  :init
  (setq hl-todo-wrap-movement t)
  (global-hl-todo-mode))

(use-package diff-hl
  :init (global-diff-hl-mode) (diff-hl-margin-mode)
  :config ;; https://github.com/dgutov/diff-hl/#magit
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :general ("C-x v a" 'diff-hl-amend-mode))
(use-package dired-git-log :straight (:host github :repo "amno1/dired-git-log")
  :after dired
  :hook ((dired-mode . dired-git-log-mode))
  :general (:keymaps 'dired-mode-map ")" 'dired-git-log-mode)
  :custom (dired-git-log-auto-hide-details-p nil))
;; TODO: goto-chg.el
(use-package git-link :general ("C-x v G" 'git-link)) ; github link at point

(use-package lua-mode :defer 3)         ; langs: scripting / config
(use-package markdown-mode)
(use-package json-mode)
;; (use-package org :straight (:type built-in)
;;   :defer 4
;;   :config (use-package :defer 4 toc-org))
(use-package i3wm-config-mode)
(use-package rust-mode                  ; compiled
  :config
  (use-package cargo)
  (use-package cargo-mode))

(use-package outshine
  :general
  (:keymaps 'outshine-mode-map
            "M-p" 'outline-previous-visible-heading
            "M-n" 'outline-next-visible-heading
            [remap consult-imenu] 'consult-outline))
;; Local Variables:
;; eval: (outshine-mode)
;; End:

                                        ; automatically make scripts executable
(add-hook 'after-save 'executable-make-buffer-file-executable-if-script-p)
(setq executable-prefix-env t)

(general-add-hook                       ; this seems like an upstream bug?
 '(conf-mode-hook emacs-lisp-mode-hook)
 (lambda () (run-mode-hooks 'prog-mode-hook)))

;;;; Emacs-as-XYZ
(load "config-notmuch.el" :noerror)     ; email client

(use-package man :straight (:type built-in)
  :custom (Man-width-max nil))

(use-package magit                      ; version control
  :general
  ("C-x   g"   'magit-status
   "C-x C-g"   'magit-status
   "C-x C-M-g" 'magit-list-repositories)
  :custom
  (magit-repository-directories
   `(("~/dotfiles" . 0)
     ("~/dotfiles-google" . 0)
     ("~/oss" . 1)))
  (magit-log-auto-more t)
  (magit-log-margin '(t "%a %b %d %Y" magit-log-margin-width t 18)))

(use-package dired-hide-dotfiles        ; file manager
  :general (:keymaps 'dired-mode-map "." 'dired-hide-dotfiles-mode))

(use-package vterm :defer 2)              ; terminal in Emacs
;; :config
;; (add-to-list 'vterm-keymap-exceptions "M-i")
(use-package vterm-toggle
  :defer 2
  :general
  ("<f2>" 'vterm-toggle)
  (:keymaps 'vterm-mode-map "<f2>" 'vterm-toggle))

(use-package xt-mouse :config (xterm-mouse-mode)) ; Emacs in terminal

(use-package bluetooth)                 ; Bluetooth device manager?!

;;;; Epilogue
(use-package no-littering               ; Emacs, stop littering!❗!
  :init
  (setq no-littering-etc-directory (f-join user-emacs-directory "lisp/"))
  :config
  (require 'recentf)
  (add-list-to-list 'recentf-exclude '(no-littering-etc-directory
                                       no-littering-var-directory))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package emacs                      ; emacs prefs
  :custom
  (confirm-kill-processes nil)
  (use-short-answers t)
  (inhibit-startup-screen t)
  (initial-scratch-message "")
  (uniquify-buffer-name-style 'forward)
  (custom-file (f-join user-emacs-directory "lisp/custom.el"))
  :config (load custom-file :noerror))  ; customize is still useful
(use-package server :config (unless (server-running-p) (server-start)))
(use-package restart-emacs :general ("C-x M-c" 'restart-emacs))

(load "specific.el" :noerror)         ; host-specific config
