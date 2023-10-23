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
(use-package f)

(use-package benchmark-init             ; start benchmark
  :hook (after-init . (lambda ()
                        (message "Emacs loaded in %s" (emacs-init-time))
                        (benchmark-init/deactivate))))
(use-package gcmh :init (gcmh-mode))    ; GC magic hack: gitlab.com/koral/gcmh

(use-package no-littering               ; must be before load path
  :init
  (setq no-littering-etc-directory (f-join user-emacs-directory "lisp/"))
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory))
  (no-littering-theme-backups))

(add-to-list 'load-path (f-join user-emacs-directory "lisp/"))
(load "config-fns.el")                  ; useful function definitions

(use-package s)         ; host identification. (system-name) is lacking the "-f"
(let ((host (s-trim (shell-command-to-string "hostname -f"))))
  (defconst my/laptop?      (not (not (string-match-p    "roam" host))))
  (defconst my/wayland?     (not (not (getenv "WAYLAND_DISPLAY"))))
  (defconst my/work?        (not (not (string-match-p  "\.com$" host))))
  (defconst my/workstation? (and my/work? (not my/laptop?))))

(use-package general)                   ; keybinds

(use-package emacs                      ; emacs prefs
  :custom
  (confirm-kill-processes nil)
  (use-short-answers t)
  (inhibit-startup-screen t)
  (initial-scratch-message "")
  (uniquify-buffer-name-style 'forward)
  (ad-redefinition-action 'accept)

  ;; custom custom.el file location for maximum custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :config (load custom-file :noerror))

(use-package server :config (unless (server-running-p) (server-start)))
(use-package restart-emacs
  :general ("C-x M-c" 'restart-emacs)
  :custom (restart-emacs-daemon-with-tty-frames-p t))

;;;; UI / UX
(use-package doom-themes                ; theme collection
  :config (load-theme 'doom-dark+ t))

;;;; Interface
(tool-bar-mode -1) (menu-bar-mode -1)   ; just use F10
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(context-menu-mode)                     ; this is good tho
(global-hl-line-mode)                   ; always good to keep track

(use-package defrepeater)
(general-def
  "C-x C-m"   'execute-extended-command ; more convenient than M-x
  "C-x m"     'execute-extended-command
  "C-x C-M-c" 'save-buffers-kill-emacs
  "M-0"       'delete-window            ; buffers and windows
  "M-k"       (lambda () (interactive) (kill-buffer nil))
  "C-x M-k"   'kill-other-buffers       ; formerly kmacro-keymap
  "C-x C-M-k" 'kmacro-keymap
  "C-x ="     'balance-windows
  "C-x C-="   'balance-windows          ; redundancy
  "C-x +"     'what-cursor-position     ; former C-x =
  "C-x M-="   'text-scale-adjust        ; former C-x C-=
  "C-c M-S"   'scroll-bar-mode)         ; occasionally useful (e.g. w3m)

(general-def
  "M-1"       'delete-other-windows
  "M-2"       'split-window-below
  "M-3"       'split-window-right
  "M-o"       (lambda () (interactive) (other-window +1))
  "M-i"       (lambda () (interactive) (other-window -1)))

(use-package which-key                  ; useful shortcut reminders
  :init (which-key-setup-side-window-bottom) (which-key-mode)
  :custom
  (which-key-idle-secondary-delay 0.01)
  (which-key-show-docstrings t))

(use-package zoom-window :general ("C-x C-z" 'zoom-window-zoom))

(use-package ace-window
  :general ([remap other-window] 'ace-window)
  :custom (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
(use-package avy
  :init (avy-setup-default)
  :general
  ([remap goto-char] 'avy-goto-char-2
   "M-j" 'avy-goto-char-timer))

(use-package windmove
  :init (windmove-default-keybindings))

(use-package doom-modeline              ; modeline
  :init
  (setq doom-modeline-support-imenu t
        doom-modeline-enable-word-count t
        doom-modeline-buffer-file-name-style 'truncate-with-project)
  (column-number-mode)
  (doom-modeline-mode)
  :hook
  ((after-change-major-mode . doom-modeline-conditional-buffer-encoding)))
(use-package all-the-icons)

(use-package helpful                    ; improved help windows
  :general
  ([remap describe-command]  'helpful-command)
  ([remap describe-function] 'helpful-callable)
  ([remap describe-key]      'helpful-key)
  ([remap describe-symbol]   'helpful-symbol)
  ([remap describe-variable] 'helpful-variable))

(use-package popper                     ; popups
  :general ("M-`"   'popper-toggle-latest)
  :custom (popper-reference-buffers
           '("\\*Messages\\*"
             "Output\\*$"
             "\\*Async Shell Command\\*"
             help-mode
             helpful-mode
             "\\*Apropos\\*"))
  :init (popper-mode) (popper-echo-mode)) ; For echo area hints

;; completion
(setq tab-always-indent 'complete
      ;; TAB cycle if there are only few candidates
      completion-cycle-threshold 3
      enable-recursive-minibuffers t)
(use-package corfu
  :init (global-corfu-mode)
  :custom (corfu-auto t)
  :general
  (:keymaps 'corfu-map
            "M-m" 'corfu-move-to-minibuffer
            "M-d" 'corfu-doc-toggle
            "M-p" 'corfu-doc-scroll-down
            "M-n" 'corfu-doc-scroll-up))
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
(use-package vertico-directory :after vertico
  :straight (:type built-in)
  :load-path "var/straight/repos/vertico/extensions/"
  :general (:keymaps 'vertico-map
                     "RET"    'vertico-directory-enter
                     "C-k"    'vertico-directory-enter
                     "DEL"    'vertico-directory-delete-char
                     "C-l"    'vertico-directory-delete-char
                     "M-DEL"  'vertico-directory-delete-word)
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
(use-package vertico-repeat    :after vertico
  :straight (:type built-in)
  :load-path "var/straight/repos/vertico/extensions/"
  :hook ((minibuffer-setup . vertico-repeat-save))
  :general ("C-x C-r" 'vertico-repeat))
(use-package vertico-mouse     :after vertico
  :straight (:type built-in)
  :load-path "var/straight/repos/vertico/extensions/"
  :config (vertico-mouse-mode))

(use-package consult
  :general                              ; remap some standard commands
  ("C-x M-:"   'consult-complex-command
   "C-x b"     'consult-buffer
   "C-x p b"   'consult-project-buffer
   "C-o"       'consult-imenu
   "C-h a"     'describe-symbol
   "M-y"       'consult-yank-pop
   "C-M-s"     'consult-ripgrep
   "C-x M-f"   'consult-fd
   "C-x C-M-f" 'consult-fd              ; redundant
   "C-c C-M-d" 'my/consult-fd-dotfiles
   [remap goto-line] 'consult-goto-line)
  (:keymaps 'isearch-mode-map
            "C-o" 'consult-line)
  :init
  (defun my/consult-fd-dotfiles ()
    "consult-fd on dotfiles repos"
    (interactive) (consult-fd "~/.dots" ""))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (xref-show-xrefs-function       #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "<")
  :config
  (recentf-mode)

  (consult-customize
   consult-line
   :add-history (seq-some #'thing-at-point '(region symbol)))
  (defalias 'consult-line-thing-at-point 'consult-line)
  (consult-customize
   consult-line-thing-at-point
   :initial (thing-at-point 'symbol))
  (general-def "C-M-o" 'consult-line-thing-at-point)

  (consult-customize consult-theme :preview-key '(:debounce 0.5 any)))

;; TODO: kill existing non-dotfiles buffer to allow magit
(general-def     ; not consult related, but an extension of my/consult-fd-config
  "C-c M-d i" 'my/config-open-init-el
  "C-c M-d s" 'my/config-open-sway
  "C-c M-d z" 'my/config-open-zshrc
  "C-c M-d w" 'my/config-open-wezterm)

(use-package bufler                     ; very nice buffer management overview
  :general
  ("C-x C-b" 'bufler-switch-buffer
   "C-x M-b" 'bufler))

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
  :general ("C-." 'embark-act
            "C-h B" 'embark-bindings)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  :general (:keymaps 'vertico-map "C-c C-e" 'embark-export))
(use-package link-hint
  :general ("C-c l o" 'link-hint-open-link))

;;;; Editing
(setq set-mark-command-repeat-pop t)    ; can keep C-u C-SPC C-SPC C-SPC...
(delete-selection-mode)                 ; typing overwrites selection
(save-place-mode)                       ; remember buffer location
(global-auto-revert-mode)

(general-def
  "C-c r" 'revert-buffer
  "M-z"   'toggle-truncate-lines        ; far more useful than C-x x t
  "C-M-z" 'zap-to-char)

(use-package sudo-edit)
(use-package so-long :init (global-so-long-mode)) ; long file handling

;; TODO; binds conflict with outshine
(use-package move-text                  ; does what is says
  :init (move-text-default-bindings))

(use-package hungry-delete              ; delete consecutive whitespace
  :init (global-hungry-delete-mode)
  :custom (hungry-delete-join-reluctantly t)) ; leave a space between words

(use-package mwim                       ; better C-a/C-e
  :general
  ([remap move-beginning-of-line] 'mwim-beginning-of-code-or-line
   [remap move-end-of-line]       'mwim-end-of-code-or-line))

(use-package iedit                      ; replace
  :general ("C-x C-;" 'iedit-mode))
(use-package visual-regexp
  :general
  ([remap query-replace-regexp] 'vr/query-replace)
  ([remap query-replace] 'vr/query-replace))

(use-package ialign
  :general ("C-x l" 'ialign)) ; interactive align regexp

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
  :general ("C-=" 'er/expand-region))

(use-package deadgrep)                  ; ripgrep UI

(use-package ws-butler                  ; delete trailing whitespace
  :custom (ws-butler-keep-whitespace-before-point nil)
  :config (ws-butler-global-mode))
(use-package column-enforce-mode)       ; highlight text past fill column

(use-package aggressive-indent          ; keep indented
  :hook ((emacs-lisp-mode . aggressive-indent-mode))
  :custom (aggressive-indent-sit-for-time 0.5)) ; slow it down

(use-package flyspell                   ; spellcheck
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :custom (ispell-dictionary "canadian")
  :general (:keymaps                    ; default binds are a little overzealous
            'flyspell-mode-map "C-," nil "C-." nil "C-;" nil "C-M-i" nil))

;;;; Programming
(setq-default indent-tabs-mode nil)     ; never tabs to indent
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq vc-follow-symlinks t              ; don't prompt
      vc-make-backup-files t
      comment-auto-fill-only-comments t)
(general-def
  "C-;"   'comment-line
  "C-x ;" (defrepeater 'comment-line))
(toggle-text-mode-auto-fill)
(add-hook 'prog-mode-hook 'auto-fill-mode)

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
  :init (require 'smartparens-config) (smartparens-global-mode)
  :general ("M-D" 'sp-splice-sexp))
(use-package rainbow-mode       :hook prog-mode)

(use-package string-inflection        ; toggle underscore -> UPCASE -> CamelCase
  :general (:keymaps '(prog-mode-map c-mode-base-map sh-mode-map)
                     "C-c C-u" 'string-inflection-cycle))

(use-package auto-highlight-symbol      ; highlight symbols
  :init (global-auto-highlight-symbol-mode))

;; VC
(use-package git-gutter
  :init (global-git-gutter-mode)
  (defrepeater 'git-gutter:previous-hunk)
  (defrepeater 'git-gutter:next-hunk)
  :custom
  (git-gutter:handled-backends '(git hg bzr svn))
  (git-gutter:hide-gutter t)
  (git-gutter:update-interval 2)
  :general
  ("C-x v =" 'git-gutter:popup-hunk
   "C-c p"   'git-gutter:previous-hunk-repeat
   "C-c n"   'git-gutter:next-hunk-repeat
   "C-x v s" 'git-gutter:stage-hunk
   "C-x v r" 'git-gutter:revert-hunk)
  :config (add-list-to-list 'git-gutter:update-commands
                            '(switch-to-buffer consult-buffer)))

(use-package dired-git-log :straight (:host github :repo "amno1/dired-git-log")
  :after dired
  :hook ((dired-mode . dired-git-log-mode))
  :general (:keymaps 'dired-mode-map ")" 'dired-git-log-mode)
  :custom (dired-git-log-auto-hide-details-p nil))
;; TODO: conflict with embark
;; (use-package goto-chg
;;   :general ("C-." 'goto-last-change
;;             "C-," 'goto-last-change-reverse))
(use-package git-link :general ("C-x v G" 'git-link)) ; github link at point

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(general-def "<f8>"                     ; compilation
  (defun my/switch-to-compilation-buffer ()
    "Switch to compilation buffer."
    (interactive) (switch-to-buffer "*compilation*")))
(general-def :keymaps '(flymake-mode-map c++-mode-map)
  "C-c C-e"   #'flymake-show-buffer-diagnostics
  "C-c C-M-e" #'flymake-show-project-diagnostics
  "C-c C-n"   #'flymake-goto-next-error
  "C-c C-p"   #'flymake-goto-prev-error)

(use-package lua-mode :defer 3)         ; langs: scripting / config
(use-package markdown-mode)
(use-package json-mode)
;; (use-package org :straight (:type built-in)
;;   :defer 4
;;   :config (use-package :defer 4 toc-org))
(use-package yaml-mode)
(use-package i3wm-config-mode)
(use-package sql-indent)
(use-package rust-mode                  ; compiled
  :config
  (use-package cargo)
  (use-package cargo-mode))
(use-package cc-mode :straight (:type built-in) ; C++
  :after smartparens
  :general (:keymaps 'c-mode-base-map "C-c C-o"
                     (defrepeater
                       (lambda () (interactive)
                         (ff-find-other-file nil 'ignore-include))))
  :hook ((c++-mode . (lambda () (c-set-offset 'innamespace [0]))))
  :config
  (general-add-hook '(c-mode-hook c++-mode-hook)
                    (lambda () (add-hook 'before-save-hook
                                         'clang-format-buffer nil :local)))
  (sp-local-pair 'c++-mode "<" ">" :when '(sp-point-after-word-p))
  (general-unbind :keymaps 'c-mode-base-map "TAB")) ; really an upstream issue

(use-package outshine
  :general (:keymaps 'outshine-mode-map
                     [remap consult-imenu] 'consult-outline))

(add-hook 'after-save-hook              ; automatically make scripts executable
          'executable-make-buffer-file-executable-if-script-p)
(setq executable-prefix-env t)

(general-add-hook                       ; this seems like an upstream bug?
 '(conf-mode-hook emacs-lisp-mode-hook)
 (lambda () (run-mode-hooks 'prog-mode-hook)))

;; imenu headers for this config and other config-local evals
(use-package outshine
  :general
  (:keymaps 'outshine-mode-map
            "M-p" 'outline-previous-visible-heading
            "M-n" 'outline-next-visible-heading
            [remap consult-imenu] 'consult-outline))

;;;; Emacs-as-XYZ
(load "config-notmuch.el" :noerror)     ; email client

(use-package man :straight (:type built-in) ; man(1)
  :custom (Man-width-max nil)
  ;; since man is used as a substitute for less(1), having vi-like
  ;; search is a useful mnemonic (and -word seems useful for man)
  :general (:keymaps 'Man-mode-map "/" 'isearch-forward-word))

(use-package magit                      ; version control
  :general
  ("C-x   g"   'magit-status
   "C-x C-g"   'magit-status
   "C-x C-M-g" 'magit-list-repositories)
  :custom
  (magit-repository-directories `(("~/.dots/dotfiles" . 0)
                                  ("~/.dots/dotfiles-google" . 0)))
  (magit-log-auto-more t)
  (magit-log-margin '(t "%a %b %d %Y" magit-log-margin-width t 18)))

(use-package dired-hide-dotfiles        ; file manager
  :general (:keymaps 'dired-mode-map "." 'dired-hide-dotfiles-mode))

(use-package vterm :defer 2             ; terminal in Emacs
  :custom
  (vterm-always-compile-module t)
  :general
  (:keymaps 'vterm-mode-map
            "M-1"  'delete-other-windows ; re-bind these
            "M-2"  'split-window-below
            "M-3"  'split-window-right
            "M-o"  (lambda () (interactive) (other-window +1))
            "M-i"  (lambda () (interactive) (other-window -1))
            ;; terminal binds
            "C-M-]" 'query-replace-regexp))
(use-package vterm-toggle
  :after vterm
  :general ("<f2>" 'vterm-toggle))

(use-package xt-mouse :config (xterm-mouse-mode)) ; Emacs in terminal

(use-package bluetooth)                 ; Bluetooth device manager?!

(use-package edit-server          ; Edit with Emacs: edit web browser text boxes
  :init (edit-server-start)
  :hook ((edit-server-start . (lambda () (auto-fill-mode -1)))))

;;;; Epilogue
(load "specific.el" :noerror)           ; host-specific config
