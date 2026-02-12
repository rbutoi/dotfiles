;;; init-eglot.el --- Eglot (LSP)  -*- lexical-binding: t; -*-

(add-hook 'eglot-managed-mode-hook
          (lambda () (eglot-inlay-hints-mode -1))) ; distracting

(use-package eglot-python-preset
  :ensure (:host github :repo "mwolson/eglot-python-preset") ; should be on MELPA but isn't for some reason
  :ensure-system-package ty
  :after eglot
  :custom (eglot-python-preset-lsp-server 'ty)
  :config (eglot-python-preset-setup))

(general-add-hook
 '(js-base-mode-hook
   typescript-base-mode-hook
   svelte-mode-hook
   terraform-mode-hook
   c++-mode-hook
   go-mode-hook
   rust-mode-hook)
 #'eglot-ensure)
(setq c++-ts-mode-hook  c++-mode-hook   ; needed: https://github.com/renzmann/treesit-auto?tab=readme-ov-file#keep-track-of-your-hooks
      go-ts-mode-hook   go-mode-hook
      rust-ts-mode-hook rust-mode-hook
      eglot-autoshutdown t)
(general-def :keymaps 'eglot-mode-map
  "C-c r"  'eglot-rename
  "C-c a"  'eglot-code-actions
  "<f7>"   'eglot-momentary-inlay-hints)

(use-package eldoc-mouse
  :ensure (:host github :repo "huangfeiyu/eldoc-mouse")
  :general
  (:keymaps 'eldoc-mouse-mode-map
            "<f1> <f1>" 'eldoc-mouse-pop-doc-at-cursor)
  :hook eldoc-mode)

;; TODO: doesn't work
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(svelte-mode . ("svelteserver" "--stdio"))))


(provide 'init-eglot)
