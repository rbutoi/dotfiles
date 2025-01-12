;; init-programming.el - Programming  -*- lexical-binding: t; -*-

(setopt indent-tabs-mode nil            ; never tabs to indent
        tab-always-indent 'complete
        vc-follow-symlinks t
        tab-width 2)
(add-hook 'prog-mode-hook
          (defun my/prog-mode-hook ()
            (display-line-numbers-mode))) ; duh

(add-hook 'eglot-managed-mode-hook
          (lambda () (eglot-inlay-hints-mode -1)))
(general-add-hook '(python-ts-mode-hook
                    typescript-ts-base-mode-hook
                    c++-ts-mode-hook)
                  #'eglot-ensure)

(defun my/search-gh-web ()
  "Search GitHub repos in browser"
  (interactive)
  (browse-url
   (concat "https://github.com/search?type=code&q="
           (url-hexify-string
            (read-from-minibuffer "GitHub code search on web: "
                                  (thing-at-point 'symbol)
                                  nil
                                  nil
                                  'my/gh-web-searches)))))

;;;;;;;;;;;;;;
;; keybinds ;;
;;;;;;;;;;;;;;

(general-def
  "C-;"   'comment-line
  "C-x ;" (defrepeater 'comment-line))

;;;;;;;;;;;;;;;;;;;;;;
;; extenal packages ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)

  ;; https://genehack.blog/2024/02/fixing-an-emacs-typescript-ts-mode-problem/
  ;; this fixes a problem where v0.20.4 of this grammar blows up with emacs
  (defvar genehack/tsx-treesit-auto-recipe
    (make-treesit-auto-recipe
     :lang 'tsx
     :ts-mode 'tsx-ts-mode
     :remap '(typescript-tsx-mode)
     :requires 'typescript
     :url "https://github.com/tree-sitter/tree-sitter-typescript"
     :revision "v0.20.3"
     :source-dir "tsx/src"
     :ext "\\.tsx\\'")
    "Recipe for libtree-sitter-tsx.dylib")
  (add-to-list 'treesit-auto-recipe-list genehack/tsx-treesit-auto-recipe)

  (defvar genehack/typescript-treesit-auto-recipe
    (make-treesit-auto-recipe
     :lang 'typescript
     :ts-mode 'typescript-ts-mode
     :remap 'typescript-mode
     :requires 'tsx
     :url "https://github.com/tree-sitter/tree-sitter-typescript"
     :revision "v0.20.3"
     :source-dir "typescript/src"
     :ext "\\.ts\\'")
    "Recipe for libtree-sitter-typescript.dylib")
  (add-to-list 'treesit-auto-recipe-list genehack/typescript-treesit-auto-recipe))

(use-package treemacs
  ;; :hook (emacs-startup . treemacs)

  :config
  (treemacs-project-follow-mode)
  (treemacs-git-commit-diff-mode))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))
(with-eval-after-load 'magit
  (use-package treemacs-magit))

(use-package string-inflection        ; toggle underscore -> UPCASE -> CamelCase
  :general (:keymaps '(prog-mode-map c-mode-base-map sh-mode-map)
                     "C-c C-u" 'string-inflection-cycle))

(use-package auto-highlight-symbol      ; highlight symbols
  :diminish
  :config
  (with-eval-after-load 'yaml-mode
    (add-to-list 'ahs-modes 'yaml-mode))
  (with-eval-after-load 'terraform-mode
    (add-to-list 'ahs-modes 'terraform-mode))
  (global-auto-highlight-symbol-mode))

(use-package corfu			; inline completions
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 1.5))

;; TODO: cape?

;; TODO: make work
;; (use-package copilot                    ; GitHub Copilot
;;   :ensure (:host github :repo "copilot-emacs/copilot.el")
;;   :hook (prog-mode . copilot-mode))



(provide 'init-programming)
