;;; init-eglot.el --- Eglot (LSP)  -*- lexical-binding: t; -*-

(use-package eglot :ensure nil
  :hook ((eglot-managed-mode . (lambda () (eglot-inlay-hints-mode -1))) ; distracting
         (fish-mode       . eglot-ensure)
         (js-base-mode    . eglot-ensure)
         (typescript-base-mode . eglot-ensure)
         (svelte-mode     . eglot-ensure)
         (terraform-mode  . eglot-ensure)
         (c++-mode        . eglot-ensure)
         (go-mode         . eglot-ensure)
         (rust-mode       . eglot-ensure))
  :custom (eglot-autoshutdown t)
  :general
  (:keymaps 'eglot-mode-map
            "C-c r"  'eglot-rename
            "C-c a"  'eglot-code-actions
            "<f7>"   'eglot-momentary-inlay-hints))

(use-package eglot-python-preset
  :defer 1
  :ensure-system-package ty
  :after eglot
  :custom (eglot-python-preset-lsp-server 'ty)
  :config (eglot-python-preset-setup))

(use-package eldoc-mouse
  :disabled                             ; TODO: slightly buggy
  :vc (:url "https://github.com/huangfeiyu/eldoc-mouse" :rev :newest)
  :general
  (:keymaps 'eldoc-mouse-mode-map
            "<f1> <f1>" 'eldoc-mouse-pop-doc-at-cursor)
  :hook eldoc-mode)


(provide 'init-eglot)
