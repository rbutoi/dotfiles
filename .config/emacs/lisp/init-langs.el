;;; init-langs.el --- Programming languages  -*- lexical-binding: t; -*-

;; Web
(use-package typescript-mode)
(use-package svelte-mode
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(svelte-mode . ("svelteserver" "--stdio")))))
(use-package json-mode)
(use-package jq-mode)
(use-package csv-mode)
(setopt js-indent-level tab-width)

;; Markup and config
(use-package markdown-mode)
(use-package grip-mode                  ; markdown preview
  :config (setq grip-command 'auto)     ; auto, grip, go-grip or mdopen
  :general (:keymaps 'markdown-mode-command-map
                     "g" 'grip-mode))
(use-package yaml-mode)
(use-package dockerfile-mode)
(use-package terraform-mode)
(use-package i3wm-config-mode)
(use-package nix-mode)

;; Systems programming
(use-package rust-mode)
(use-package go-mode)
(use-package zig-mode)
(use-package swift-mode)

;; Scripting
(use-package lua-mode)
(use-package fish-mode
  :ensure-system-package fish-lsp
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(fish-mode . ("fish-lsp" "start")))))

;; Build systems
(use-package just-mode)
(use-package cmake-mode)


(provide 'init-langs)
