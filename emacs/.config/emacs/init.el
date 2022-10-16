;; -*- lexical-binding: t; -*-
;; init.el - Emacs config

;;;; Prologue
;; package management: straight.el / use-package
(setq straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
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

;;;; UI / UX: font is set using customize, in custom.el
(use-package general)

(use-package doom-themes
  :config
  (load-theme 'doom-gruvbox t)
  (add-hook 'window-setup-hook
            (lambda ()
              (unless (display-graphic-p) ; show terminal's transparent background
                (set-face-background 'default "unspecified-bg")))))

(use-package which-key                  ; useful shortcut reminders
  :config (which-key-setup-side-window-bottom) (which-key-mode))

(general-def                            ; buffers and windows
  "M-l"       (lambda () (interactive) (select-window (get-mru-window t t t)))
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
  :general ("C-c C-j" 'avy-resume)
  :config (avy-setup-default))

(use-package doom-modeline              ; modeline
  :init (use-package all-the-icons)
  (column-number-mode)
  (setq doom-modeline-support-imenu t
        doom-modeline-enable-word-count t)
  :config (doom-modeline-mode))

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

(use-package ivy                        ; minibuffer
  :general
  (:keymaps 'ivy-minibuffer-map
            "C-k"     'ivy-alt-done   ; C-j is used by tmux
            "C-M-k"   'ivy-kill-line
            "C-M-i"   'ivy-insert-current) ; M-i used to change windows
  :config
  (use-package wgrep)
  (ivy-mode)
                                        ; .. can be replaced by DEL/C-l, but . is still useful for e.g. dired
  (setq ivy-extra-directories '(".")))
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
   "C-c M-c"   (defun my/counsel-fd-file-jump-config () (interactive)
                      (counsel-fd-file-jump "" "~/.config/"))
   "C-M-o"     'swiper-thing-at-point)
  (:keymaps 'counsel-find-file-map
            "C-l" 'counsel-up-directory)
  :config
  (counsel-mode)
  (use-package counsel-fd
    :config
    (setq counsel-fd-command "fd --hidden --color never --full-path "))
  (defun my/counsel-fd-file-jump-ask () (interactive)
         (let ((current-prefix-arg '(4)))
           (counsel-fd-file-jump)))
  (defun my/counsel-rg-symbol-at-point () (interactive)
         (counsel-rg (thing-at-point 'symbol))))
(use-package prescient                ; remembers
  :config (setq completion-styles '(prescient basic)))
(use-package ivy-prescient :config (ivy-prescient-mode t))
(use-package marginalia :init (marginalia-mode)) ; rich annotations

;;;; Editing
(use-package mwim
  :general
  ([remap move-beginning-of-line] 'mwim-beginning-of-code-or-line)
  ([remap move-end-of-line]       'mwim-end-of-code-or-line))

(use-package undo-tree                  ; visual undo
  :general
  ("C-z" 'undo-tree-undo)
  :config
  (setq
   undo-tree-history-directory-alist '(("." . "~/.cache/emacs-undo-tree/"))
   undo-tree-visualizer-diff t
   undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode))

(use-package expand-region
  :general ("M-=" 'er/expand-region))   ; terminal-friendly bind

(use-package swiper                     ; better search
  :general (:keymaps 'isearch-mode-map
                     "C-o" 'swiper-from-isearch))
(use-package counsel-edit-mode          ; edit while searching
  :config (counsel-edit-mode-setup-ivy))
(use-package deadgrep)                  ; ripgrep UI

(use-package ws-butler                  ; delete trailing whitespace
  :config
  (setq ws-butler-keep-whitespace-before-point nil)
  (ws-butler-global-mode))

(use-package aggressive-indent          ; keep indented
  :hook ((emacs-lisp-mode . aggressive-indent-mode))
  :config (setq aggressive-indent-sit-for-time 0.5))

;;;; Programming
(setq-default indent-tabs-mode nil)     ; never tabs to indent
(setq vc-follow-symlinks t)             ; don't prompt
(general-add-hook '(text-mode-hook prog-mode-hook)
                  '(display-line-numbers-mode hl-line-mode))

(use-package editorconfig
  :config (editorconfig-mode 1))

;; (use-package tree-sitter)               ; TODO: something

;; (use-package yasnippet
;;   :general (yas-minor-mode-map
;;             :states 'insert
;;             "TAB" 'nil
;;             "C-TAB" 'yas-expand)
;;   :hook ((prog-mode) . yas-minor-mode)
;;   :config (yas-reload-all))

(use-package smartparens                ; parentheses
  :config (require 'smartparens-config) (smartparens-global-mode))
(use-package rainbow-delimiters :hook prog-mode)

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
(use-package git-link :general ("C-c g l" 'git-link)) ; github link at point

(use-package lua-mode)                  ; langs: scripting / config
(use-package markdown-mode)
(use-package org :straight (:type built-in) :config (use-package toc-org))
(use-package rust-mode                  ; compiled
  :config
  (use-package cargo)
  (use-package cargo-mode))

                                        ; automatically make scripts executable
(add-hook 'after-save 'executable-make-buffer-file-executable-if-script-p)

(defun run-prog-hooks () (run-mode-hooks 'prog-mode-hook)) ; this seems like an
(general-add-hook '(conf-mode elisp-mode) 'run-mode-hooks) ; upstream bug?

;;;; Emacs-as-XYZ
(use-package dired-hide-dotfiles        ; file manager
  :general (:keymaps 'dired-mode-map "." 'dired-hide-dotfiles-mode))

(use-package vterm                      ; terminal in Emacs
  :config
  (add-to-list 'vterm-keymap-exceptions "M-i"))
(use-package vterm-toggle
  :general
  ("<f2>" 'vterm-toggle)
  (:keymaps 'vterm-mode-map "<f2>" 'vterm-toggle))
(use-package xt-mouse :config (xterm-mouse-mode)) ; Emacs in terminal

;;;; Epilogue
(load "config-fns.el")                  ; useful function defininitons

(use-package no-littering
  :init
  (setq no-littering-etc-directory (f-join user-emacs-directory "lisp/"))
  :config
  (require 'recentf)
  (add-list-to-list 'recentf-exclude '(no-littering-etc-directory
                                       no-littering-var-directory))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package emacs                      ; emacs prefs
  :config
  (setq
   confirm-kill-processes nil
   vc-make-backup-files t
   savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
   custom-file (f-join user-emacs-directory "lisp/custom.el")) ; customize is
  (load custom-file :noerror)                                  ; still useful
  (savehist-mode)                       ; save minibuffer hostory
  (tool-bar-mode -1) (menu-bar-mode -1) ; just use F10
  (defalias 'yes-or-no-p 'y-or-n-p))    ; why make you type the whole word?
(use-package server :config (unless (server-running-p) (server-start)))
(use-package restart-emacs
  :general
  ("C-x C-M-c" 'save-buffers-kill-emacs)
  ("C-x M-c" 'restart-emacs))

(load "specific.el"   :noerror)         ; host-specific config
