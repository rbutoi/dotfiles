;; -*- lexical-binding: t; -*-
;; init-programming.el - Emacs config for programming

(setq-default indent-tabs-mode nil)     ; never tabs to indent
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq vc-follow-symlinks t)             ; don't prompt

(general-def
  "C-;"   'comment-line
  ;; "C-x ;" (defrepeater 'comment-line)
  )

(use-package elisp-autofmt		; elisp
  :config
  (with-system darwin
    (setq elisp-autofmt-python-bin "python3"))
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

(use-package terraform-mode)		; tf

(use-package magit			; version control
  :general
  ("C-x C-g"   'magit-status
   "C-x C-M-g" 'magit-list-repositories)
  :custom
  (magit-repository-directories `(("~/.dots/dotfiles" . 0)
                                  ("~/dev" . 1)))
  ;; (magit-log-auto-more t) TODO: keep?
  ;; (magit-log-margin '(t "%a %b %d %Y" magit-log-margin-width t 18))
  )

(provide 'init-programming)
