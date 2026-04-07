;;; init-programming.el --- Programming  -*- lexical-binding: t; -*-

(setopt indent-tabs-mode nil            ; never tabs to indent
        tab-always-indent 'complete
        tab-width 2
        eldoc-idle-delay 0.2)

(defun my/search-gh-web ()
  "Search GitHub repos in browser"
  (interactive)
  (browse-url (concat
               "https://github.com/search?type=code&q="
               (url-hexify-string
                (read-from-minibuffer "GitHub code search on web: "
                                      (thing-at-point 'symbol) nil nil
                                      'my/gh-web-searches)))))

;;; AI
(use-package track-changes)
(use-package copilot                    ; GitHub Copilot
  :after (track-changes corfu)
  :defer 1              ; can't use :hook without loading copilot immediately
  :custom (copilot-idle-delay corfu-auto-delay)
  :general
  (:keymaps 'copilot-completion-map
            "<tab>" 'copilot-accept-completion
            "M-f"   'copilot-accept-completion-by-word
            "C-M-n" 'copilot-next-completion
            "C-M-p" 'copilot-previous-completion
            "C-g"   'copilot-clear-overlay)
  :config (add-hook 'prog-mode-hook #'copilot-mode))
(use-package agent-shell
  :ensure-system-package
  ((gemini . gemini-cli)
   (claude . claude-code)
   (claude-agent-acp . "pnpm install -g @zed-industries/claude-agent-acp"))
  :general ("s-l" 'agent-shell)         ; like in vscode
  :config
  (defun my/agent-shell-dot-subdir (subdir)
    (let ((cwd (agent-shell-cwd)))
      (if (string-prefix-p (expand-file-name "~/.dots/dotfiles") (expand-file-name cwd))
          (let* ((cwd (string-remove-suffix "/" cwd))
                 (sanitized (replace-regexp-in-string "/" "-" (string-remove-prefix "/" cwd))))
            (no-littering-expand-var-file-name (file-name-concat "agent-shell" sanitized subdir)))
        (agent-shell--dot-subdir-in-repo subdir)))) ; fallback to default behavior if not in ~/.dots/dotfiles

  (setopt agent-shell-dot-subdir-function #'my/agent-shell-dot-subdir))
;;;

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

(use-package rainbow-mode :hook prog-mode) ; #ff7900

(use-package apheleia                   ; auto format
  :hook (elpaca-after-init . apheleia-global-mode)
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(ruff ruff-isort))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff ruff-isort)))

(use-package compiler-explorer          ; godbolt.org
  :custom (compiler-explorer-sessions-file
           (no-littering-expand-var-file-name "compiler-explorer")))

(use-package flyover)                   ; TODO


(provide 'init-programming)
