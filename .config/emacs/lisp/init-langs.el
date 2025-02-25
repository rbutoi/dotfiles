;; init-langs.el - Programming languages  -*- lexical-binding: t; -*-

(use-package apheleia                   ; auto format
  :diminish "A"
  :init (apheleia-global-mode)
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(ruff ruff-isort))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff ruff-isort)))

(use-package lua-mode)
(use-package terraform-mode)
(use-package yaml-mode)
(use-package json-mode)
(use-package jq-mode)
(use-package i3wm-config-mode)
(use-package markdown-mode)
(use-package dockerfile-mode)
(use-package deno-ts-mode)
(use-package rust-mode)
(use-package go-mode)
(use-package just-mode)
(use-package fish-mode)


(provide 'init-langs)
