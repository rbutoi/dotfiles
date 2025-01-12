;; init-langs.el - Programming languages  -*- lexical-binding: t; -*-

(setopt python-indent-offset 2
        sh-basic-offset      2)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(general-add-hook '(conf-mode-hook yaml-mode-hook) 'my/prog-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;
;; extenal packages ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package format-all)

(use-package lua-mode)
(use-package terraform-mode)
(use-package yaml-mode)
(use-package json-mode)
(use-package jq-mode)
(use-package i3wm-config-mode)
(use-package markdown-mode)
(use-package sql-indent)
(use-package dockerfile-mode)
(use-package deno-ts-mode)
(use-package deno-fmt :hook (deno-ts-mode deno-tsx-ts-mode))
(use-package rust-mode)
(use-package go-mode)
(use-package just-mode)
(use-package fish-mode
  :hook (fish-mode . (lambda ()
                       (add-hook 'before-save-hook
                                 #'fish_indent-before-save))))
;; TODO C++ config


(provide 'init-langs)
