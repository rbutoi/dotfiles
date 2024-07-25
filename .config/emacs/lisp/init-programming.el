;; init-programming.el - Programming  -*- lexical-binding: t; -*-

(setopt indent-tabs-mode nil            ; never tabs to indent
        tab-always-indent 'complete
        vc-follow-symlinks t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode) ; duh

(toggle-text-mode-auto-fill)            ; auto-fill
(add-hook 'prog-mode-hook 'auto-fill-mode)

;;;;;;;;;;;;;;
;; keybinds ;;
;;;;;;;;;;;;;;

(general-def
  "C-;"   'comment-line
  "C-x ;" (defrepeater 'comment-line))

;;;;;;;;;;;;;;;;;;;;;;
;; extenal packages ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package string-inflection        ; toggle underscore -> UPCASE -> CamelCase
  :general (:keymaps '(prog-mode-map c-mode-base-map sh-mode-map)
                     "C-c C-u" 'string-inflection-cycle))

(use-package auto-highlight-symbol      ; highlight symbols
  :diminish
  :config (global-auto-highlight-symbol-mode))

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
