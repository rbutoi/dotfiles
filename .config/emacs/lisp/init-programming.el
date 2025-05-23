;; init-programming.el - Programming  -*- lexical-binding: t; -*-

(setopt indent-tabs-mode nil            ; never tabs to indent
        tab-always-indent 'complete
        vc-follow-symlinks t
        tab-width 2)
(general-add-hook '(prog-mode-hook text-mode-hook)
                  'display-line-numbers-mode)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(general-add-hook
 '(python-ts-mode-hook
   js-base-mode-hook
   js-ts-mode-hook
   typescript-ts-base-mode-hook
   terraform-mode-hook
   c++-ts-mode-hook
   go-ts-mode-hook
   rust-ts-mode-hook)
 #'eglot-ensure)
(add-hook 'eglot-managed-mode-hook
          (lambda () (eglot-inlay-hints-mode -1))) ; distracting

(with-eval-after-load 'diminish
  (when (fboundp 'global-completion-preview-mode) ; as of emacs 30.1
    (global-completion-preview-mode)
    (diminish 'completion-preview-mode)))

(defun my/search-gh-web ()
  "Search GitHub repos in browser"
  (interactive)
  (browse-url (concat
               "https://github.com/search?type=code&q="
               (url-hexify-string
                (read-from-minibuffer "GitHub code search on web: "
                                      (thing-at-point 'symbol) nil nil
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

(use-package disproject
  ;; Replace `project-prefix-map' with `disproject-dispatch'.
  :general (:keymaps 'ctl-x-map
                     "p" 'disproject-dispatch))

(use-package treesit-auto
  :custom (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package treemacs
  :general ("C-x C-M-SPC" 'treemacs)
  :config
  (treemacs-project-follow-mode)
  (treemacs-git-commit-diff-mode))

(with-eval-after-load 'magit
  (use-package treemacs-magit))

(use-package string-inflection        ; toggle underscore -> UPCASE -> CamelCase
  :general (:keymaps '(prog-mode-map c-mode-base-map sh-mode-map)
                     "C-c C-u" 'string-inflection-cycle))

;; TODO: use??
;; (use-package flycheck)

(use-package auto-highlight-symbol      ; highlight symbols
  :diminish
  :config
  (with-eval-after-load 'yaml-mode
    (add-to-list 'ahs-modes 'yaml-mode))
  (with-eval-after-load 'terraform-mode
    (add-to-list 'ahs-modes 'terraform-mode))
  (global-auto-highlight-symbol-mode))

;; completion

(use-package corfu			; inline completions
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :custom
  (corfu-auto t))
;; TODO: cape?

(use-package copilot                    ; GitHub Copilot
  :diminish
  :ensure (:host github :repo "copilot-emacs/copilot.el")
  :custom
  (copilot-idle-delay 0.7)
  :hook (prog-mode . copilot-mode)
  :general
  (:keymaps 'copilot-completion-map
            "<tab>" 'copilot-accept-completion
            "M-f"   'copilot-accept-completion-by-word
            "C-M-n"   'copilot-next-completion
            "C-M-p"   'copilot-previous-completion
            "C-g"   'copilot-clear-overlay))


(provide 'init-programming)
