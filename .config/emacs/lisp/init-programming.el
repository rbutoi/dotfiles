;; init-programming.el - Programming  -*- lexical-binding: t; -*-

(setopt indent-tabs-mode nil            ; never tabs to indent
        tab-always-indent 'complete
        vc-follow-symlinks t
        tab-width 2)
(general-add-hook '(prog-mode-hook text-mode-hook)
                  'display-line-numbers-mode)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(defun my/search-gh-web ()
  "Search GitHub repos in browser"
  (interactive)
  (browse-url (concat
               "https://github.com/search?type=code&q="
               (url-hexify-string
                (read-from-minibuffer "GitHub code search on web: "
                                      (thing-at-point 'symbol) nil nil
                                      'my/gh-web-searches)))))

;; eglot
(add-hook 'eglot-managed-mode-hook
          (lambda () (eglot-inlay-hints-mode -1))) ; distracting
(general-add-hook
 '(python-base-mode-hook js-base-mode-hook
                         typescript-ts-base-mode-hook
                         terraform-mode-hook c++-mode-hook go-mode-hook
                         rust-mode-hook)
 #'eglot-ensure)
(setq c++-ts-mode-hook  c++-mode-hook   ; needed: https://github.com/renzmann/treesit-auto?tab=readme-ov-file#keep-track-of-your-hooks
      go-ts-mode-hook   go-mode-hook
      rust-ts-mode-hook rust-mode-hook
      eglot-autoshutdown t)
(general-def :keymaps 'eglot-mode-map
  "C-c r"  'eglot-rename
  "C-c a"  'eglot-code-actions)

;;;;;;;;;;;;;;;;;;;;;;
;; extenal packages ;;
;;;;;;;;;;;;;;;;;;;;;;

;; completion

(with-eval-after-load 'diminish
  ;; (when (fboundp 'global-completion-preview-mode) ; as of emacs 30.1
  (global-completion-preview-mode)
  (diminish 'completion-preview-mode)) ;; )
(use-package corfu			; inline completions
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :custom
  (corfu-auto t))
;; TODO: cape?

(use-package disproject
  ;; Replace `project-prefix-map' with `disproject-dispatch'.
  :general (:keymaps 'ctl-x-map
                     "p" 'disproject-dispatch))

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

(use-package auto-highlight-symbol      ; highlight symbols
  :diminish
  :config
  (with-eval-after-load 'yaml-mode
    (add-to-list 'ahs-modes 'yaml-mode))
  (with-eval-after-load 'terraform-mode
    (add-to-list 'ahs-modes 'terraform-mode))
  (global-auto-highlight-symbol-mode))

(provide 'init-programming)

;;
;;; banished

(use-package treesit-auto
  :disabled                         ; definitely adds too much to file load time
  :custom (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package copilot                    ; GitHub Copilot
  :disabled                             ; hmm
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
