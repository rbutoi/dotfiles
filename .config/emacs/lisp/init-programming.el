;;; init-programming.el --- Programming  -*- lexical-binding: t; -*-

(setopt indent-tabs-mode nil            ; never tabs to indent
        tab-always-indent 'complete
        tab-width 2)
(general-add-hook '(prog-mode-hook text-mode-hook)
                  'display-line-numbers-mode)

(defun my/search-gh-web ()
  "Search GitHub repos in browser"
  (interactive)
  (browse-url (concat
               "https://github.com/search?type=code&q="
               (url-hexify-string
                (read-from-minibuffer "GitHub code search on web: "
                                      (thing-at-point 'symbol) nil nil
                                      'my/gh-web-searches)))))

(use-package track-changes)
(use-package copilot                    ; GitHub Copilot
  :after (track-changes corfu)
  :defer 1
  :ensure (:host github :repo "copilot-emacs/copilot.el")
  :custom
  (copilot-idle-delay corfu-auto-delay)
  :general
  (:keymaps 'copilot-completion-map
            "<tab>" 'copilot-accept-completion
            "M-f"   'copilot-accept-completion-by-word
            "C-M-n" 'copilot-next-completion
            "C-M-p" 'copilot-previous-completion
            "C-g"   'copilot-clear-overlay)
  :config
  (add-hook 'prog-mode-hook #'copilot-mode))
;; TODO: copilot-chat.el

(use-package string-inflection        ; toggle underscore -> UPCASE -> CamelCase
  :general (:keymaps '(prog-mode-map c-mode-base-map sh-mode-map)
                     "C-c C-u" 'string-inflection-cycle))

(use-package auto-highlight-symbol      ; highlight symbols
  :hook (elpaca-after-init . global-auto-highlight-symbol-mode)
  :config
  (with-eval-after-load 'yaml-mode
    (add-to-list 'ahs-modes 'yaml-mode))
  (with-eval-after-load 'terraform-mode
    (add-to-list 'ahs-modes 'terraform-mode)))

(use-package apheleia                   ; auto format
  :hook (elpaca-after-init . apheleia-global-mode)
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(ruff ruff-isort))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff ruff-isort)))

(use-package compiler-explorer
  :custom (compiler-explorer-sessions-file
           (no-littering-expand-var-file-name "compiler-explorer"))
  ;; :config (message "Compiler Explorer sessions will be saved to %s" compiler-explorer-sessions-file)
  )


(provide 'init-programming)
