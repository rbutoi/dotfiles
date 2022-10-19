;; -*- lexical-binding: t; -*-
;; init.el - Emacs config

;;;; Prologue
;; package management: straight.el / use-package
(setq
 straight-base-dir (concat user-emacs-directory "var/") ; no f-join yet
 straight-use-package-by-default t)

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
(use-package f)                         ; add local load path
(add-to-list 'load-path (f-join user-emacs-directory "lisp/"))

(let ((host (shell-command-to-string "hostname -f"))) ; host identification
  (defconst my/wayland?     (not (not (getenv "WAYLAND_DISPLAY"))))
  (defconst my/work?        (not (not (string-match-p  "google.*\.com" host))))
  (defconst my/glaptop?     (not (not (string-match-p           "roam" host))))
  (defconst my/crostini?    (not (not (string-match-p        "penguin" host))))
  (defconst my/workstation? (and my/work? (not my/glaptop?))))

;;;; UI / UX
(tool-bar-mode -1) (menu-bar-mode -1) ; just use M-` / F10
(context-menu-mode)                   ; this is good tho
(global-hl-line-mode)                 ; always good to keep track

(use-package general)                   ; keybinds

(use-package doom-themes                ; theme
  :init
  (load-theme 'doom-gruvbox t)
  (add-hook 'window-setup-hook          ; show terminal's transparent background
            (lambda ()
              (unless (display-graphic-p)
                (set-face-background 'default "unspecified-bg")))))

(use-package which-key                  ; useful shortcut reminders
  :init (which-key-setup-side-window-bottom) (which-key-mode)
  :config (setq which-key-idle-secondary-delay 0.01))

(general-def                            ; buffers and windows
  "M-0"       'delete-window)
(general-def :keymaps '(global magit-mode-map) ; just drop the M- in magit
  "M-1"       'delete-other-windows
  "M-2"       'split-window-below
  "M-3"       'split-window-right
  "M-o"       (lambda () (interactive) (other-window +1))
  "M-i"       (lambda () (interactive) (other-window -1)))
(use-package ace-window
  :general ([remap other-window] 'ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
(use-package avy
  :init (avy-setup-default)
  :general ("C-c C-j" 'avy-resume))

(use-package doom-modeline              ; modeline
  :init
  (use-package all-the-icons)
  (setq doom-modeline-support-imenu t
        doom-modeline-enable-word-count t
        doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (column-number-mode)
  (doom-modeline-mode))

(use-package helpful                    ; improved help windows
  :config (setq counsel-describe-function-function #'helpful-callable
                counsel-describe-variable-function #'helpful-variable))

(setq tab-always-indent 'complete)      ; completion

;; (use-package corfu
;;   :init
;;   (global-corfu-mode))

;; (straight-use-package
;;  '(corfu-terminal
;;    :type git
;;    :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))
;; (use-package corfu-terminal
;;   (unless (display-graphic-p)
;;     (corfu-terminal-mode +1)))

(use-package ivy                        ; minibuffer completion
  :init (ivy-mode)
  (use-package wgrep)
  ;; .. can be replaced by DEL/C-l, but . is still useful for e.g. dired
  (setq ivy-extra-directories '(".")
        ivy-wrap t)
  :general
  (:keymaps 'ivy-minibuffer-map
            "C-k"     'ivy-alt-done     ; C-j is used by tmux
            "C-M-k"   'ivy-kill-line
            "C-M-i"   'ivy-insert-current)) ; M-i used to change windows
(use-package counsel
  :general
  ("C-c C-r"   'ivy-resume
   "C-x m"     'counsel-M-x
   "C-x C-m"   'counsel-M-x
   "C-x C-b"   'counsel-switch-buffer
   "C-x b"     'counsel-buffer-or-recentf
   "C-o"       'counsel-semantic-or-imenu
   "C-M-s"     'my/counsel-rg-symbol-at-point
   "C-x f"     'my/counsel-fd-file-jump-ask
   "C-x M-f"   'counsel-fd-file-jump
   "C-c M-c"   'my/counsel-fd-config)
  (:keymaps 'counsel-find-file-map
            "C-l" 'counsel-up-directory)
  :config
  (counsel-mode)
  (defun my/counsel-rg-symbol-at-point ()
    (interactive) (counsel-rg (thing-at-point 'symbol)))
  (use-package counsel-fd
    :config
    (setq counsel-fd-command
          "fd --hidden --color never --full-path --follow -tl -ts -td ")
    (defun my/counsel-fd-config ()
      "Jump to file in ~/.config"
      (interactive) (counsel-fd-file-jump "" "~/.config/"))
    (defun my/counsel-fd-file-jump-ask ()
      "(counsel-file-jump) but ask by default"
      (interactive)
      (let ((current-prefix-arg 4))
        (call-interactively 'counsel-fd-file-jump)))))
(use-package ivy-rich
  :after counsel
  :init (ivy-rich-mode)
  :config
  (setq ivy-rich-project-root-cache-mode t)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package prescient                ; remembers
  :config
  (setq completion-styles '(prescient basic))
  (prescient-persist-mode))
(use-package ivy-prescient :config (ivy-prescient-mode))
;; (use-package marginalia :init (marginalia-mode)) ; rich annotations

;;;; Editing
(delete-selection-mode)                 ; typing overwrites selection
(use-package sudo-edit)
(use-package so-long :init (global-so-long-mode)) ; long file handling

(use-package mwim                       ; better C-a/C-e
  :general
  ([remap move-beginning-of-line] 'mwim-beginning-of-code-or-line)
  ([remap move-end-of-line]       'mwim-end-of-code-or-line))

(use-package undo-tree                  ; visual undo
  :defer 2
  :general
  ("C-z"   'undo-tree-undo)
  ("C-x u" 'undo-tree-visualize)
  :config
  (setq
   undo-tree-history-directory-alist backup-directory-alist
   undo-tree-visualizer-diff t
   undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode))

(use-package expand-region
  :general ("M-=" 'er/expand-region))   ; terminal-friendly bind

(use-package swiper                     ; better search
  :general
  ("C-M-o" 'swiper-thing-at-point)
  (:keymaps 'isearch-mode-map
	    "C-o" 'swiper-from-isearch))
(use-package ctrlf :init (ctrlf-mode))  ; another isearch replacement
(use-package counsel-edit-mode          ; edit while searching
  :config (counsel-edit-mode-setup-ivy))
(use-package deadgrep)                  ; ripgrep UI

(use-package ws-butler                  ; delete trailing whitespace
  :config
  (setq ws-butler-keep-whitespace-before-point nil)
  (ws-butler-global-mode))
(use-package column-enforce-mode        ; highlight text past fill column
  :init (global-column-enforce-mode))

(use-package aggressive-indent          ; keep indented
  :hook ((emacs-lisp-mode . aggressive-indent-mode))
  :config (setq aggressive-indent-sit-for-time 0.5))

(use-package flyspell                   ; spellcheck
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))
(use-package flyspell-correct
  :after flyspell
  :general
  (:keymaps 'flyspell-mode-map "C-;"
            'flyspell-correct-wrapper))
(use-package flyspell-correct-ivy :after flyspell-correct)

;;;; Programming
(setq-default indent-tabs-mode nil)     ; never tabs to indent
(setq vc-follow-symlinks t)             ; don't prompt
(toggle-text-mode-auto-fill)
(general-add-hook '(text-mode-hook prog-mode-hook) 'display-line-numbers-mode)
(use-package editorconfig :init (editorconfig-mode))

(use-package tree-sitter
  :defer 6
  :config
  (use-package tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (use-package yasnippet
;;   :general (yas-minor-mode-map
;;             :states 'insert
;;             "TAB" 'nil
;;             "C-TAB" 'yas-expand)
;;   :hook ((prog-mode) . yas-minor-mode)
;;   :config (yas-reload-all))

(use-package smartparens                ; parentheses
  :defer 1
  :init (require 'smartparens-config) (smartparens-global-mode))
(use-package rainbow-delimiters :hook prog-mode)
(use-package rainbow-mode       :hook prog-mode)

(use-package string-inflection        ; toggle underscore -> UPCASE -> CamelCase
  :general
  (:keymaps '(prog-mode-map c-mode-base-map sh-mode-map)
            "C-c C-u" 'string-inflection-cycle))

(use-package auto-highlight-symbol      ; highlight symbols
  :init
  (global-auto-highlight-symbol-mode)
  :general
  (:keymaps 'auto-highlight-symbol-mode-map
            "M-p"   'ahs-backward
            "M-n"   'ahs-forward
            "M-S-p" 'ahs-backward-definition
            "M-S-n" 'ahs-forward-definition))
(use-package hl-todo                   ; highlight TODOs
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
  ;; :commands (dired-git-log-auto-enable)
  :after dired
  :hook ((dired-mode         . dired-git-log-mode)
         ;; (dired-after-readin . dired-git-log-auto-enable)
         )
  :general
  (:keymaps 'dired-mode-map ")" 'dired-git-log-mode))
;; TODO: goto-chg.el
(use-package git-link :general ("C-c g l" 'git-link)) ; github link at point

(use-package lua-mode :defer 3)         ; langs: scripting / config
(use-package markdown-mode)
(use-package json-mode)
;; (use-package org :straight (:type built-in)
;;   :defer 4
;;   :config (use-package :defer 4 toc-org))
(use-package rust-mode                  ; compiled
  :config
  (use-package cargo)
  (use-package cargo-mode))

                                        ; automatically make scripts executable
(add-hook 'after-save 'executable-make-buffer-file-executable-if-script-p)
(setq executable-prefix-env t)

(general-add-hook '(conf-mode-hook emacs-lisp-mode-hook) ; this seems like an upstream bug?
                  (lambda () (run-mode-hooks 'prog-mode-hook)))

;;;; Emacs-as-XYZ
(load "config-notmuch.el" :noerror)     ; email client

(use-package magit                      ; version control
  :general
  ("C-x   g"   'magit-status
   "C-x C-g"   'magit-status
   "C-x C-M-g" 'magit-list-repositories)
  :config
  (setq magit-repository-directories
        `(("~/dotfiles" . 0)
          ("~/dotfiles-google" . 0)
          ("~/oss" . 1))
        magit-log-auto-more t
        magit-log-margin '(t "%a %b %d %Y" magit-log-margin-width t 18)))

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

;;;; epilogue
(load "config-fns.el")                  ; useful function definitions

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
  :general
  ("C-x C-M-c" 'save-buffers-kill-emacs)
  :config
  (setq
   confirm-kill-processes nil
   use-short-answers t                  ; why you make me type "yes"
   inhibit-startup-screen t
   initial-scratch-message ""
   uniquify-buffer-name-style 'forward
   vc-make-backup-files t
   savehist-file (no-littering-expand-var-file-name "savehist.el")
   savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
   custom-file (f-join user-emacs-directory "lisp/custom.el"))
  (load custom-file :noerror)           ; customize is still useful
  (savehist-mode))                      ; save minibuffer history
(use-package server :config (unless (server-running-p) (server-start)))
(use-package restart-emacs :general ("C-x M-c" 'restart-emacs))

(load "specific.el" :noerror)         ; host-specific config
