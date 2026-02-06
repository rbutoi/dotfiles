;;; init-langs.el --- Programming languages  -*- lexical-binding: t; -*-

;; TODO: weird init issues
;; diminish goes away after 2 secs (something with idle loading), needs re-eval
;; to get it back
(use-package apheleia                   ; auto format
  :diminish "A"
  :init (apheleia-global-mode)
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(ruff ruff-isort))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff ruff-isort)))

;; Web and data formats
(use-package json-mode    :defer 2)
(use-package jq-mode)
;; (use-package deno-ts-mode :defer 2)
(use-package csv-mode)
(use-package typescript-mode)
(use-package svelte-mode)

;; TODO: doesn't work
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs '(svelte-mode . ("svelteserver" "--stdio"))))

(setopt js-indent-level tab-width)

;; Markup and config files
(use-package yaml-mode)
(use-package markdown-mode)
(use-package grip-mode                  ; markdown preview
  :config (setq grip-command 'auto)     ; auto, grip, go-grip or mdopen
  :general (:keymaps 'markdown-mode-command-map
                     "g" 'grip-mode))
(use-package dockerfile-mode)
(use-package terraform-mode)
(use-package i3wm-config-mode)
;; TODO: rainbow-mode??

(with-eval-after-load 'diminish
  (diminish 'eldoc-mode))

;; Systems programming
(use-package rust-mode)
(use-package go-mode)
(use-package zig-mode)

;; Scripting languages
(use-package lua-mode)
(use-package fish-mode)

;; Build systems
(use-package just-mode)


(provide 'init-langs)
