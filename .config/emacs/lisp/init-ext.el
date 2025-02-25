;; init-ext.el - External integrations  -*- lexical-binding: t; -*-

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (when (executable-find "gls") (setopt insert-directory-program "gls")))

(xterm-mouse-mode)                      ; mouse in terminal Emacs

(use-package man                        ; man(1)
  :ensure nil
  :custom
  (Man-width-max nil)
  (Man-notify-method 'aggressive)
  :general (:keymaps 'Man-mode-map "/" 'isearch-forward-word))

(use-package dired-hide-dotfiles        ; file manager
  :general (:keymaps 'dired-mode-map "." 'dired-hide-dotfiles-mode))
(use-package dired-gitignore
  :init (dired-gitignore-global-mode t)
  :general (:keymaps 'dired-mode-map "M-." 'dired-gitignore-global-mode))


(elpaca (transient))            ; https://github.com/progfolio/elpaca/issues/343
(use-package magit              ; version control
  :hook
  (after-save . magit-after-save-refresh-status)
  :general
  ("C-x C-g"   'magit-status
   "C-x C-M-g" 'magit-list-repositories)
  (:keymaps 'magit-mode-map "M-m" 'magit-toggle-margin)
  :custom
  (magit-repository-directories `(("~/dev" . 2)
                                  ("~/oss" . 1)))
  (magit-log-auto-more t)
  (magit-pull-or-fetch t))
(use-package git-modes)
(use-package forge)

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t)
  (add-hook 'diff-hl-mode-hook
            (lambda ()
              (unless (display-graphic-p)
                (diff-hl-margin-local-mode)))))

(use-package git-link :general ("C-x v G" 'git-link)) ; github link at point

(use-package vterm                      ; terminal
  :defer 2
  :custom
  (vterm-always-compile-module t)
  :general
  (:keymaps 'vterm-mode-map
            "M-1"  'delete-other-windows ; re-bind these
            "M-2"  'split-window-below
            "M-3"  'split-window-right
            "M-o"  (cmd! (other-window +1))
            "M-i"  (cmd! (other-window -1))
            "<f5>" 'vterm-toggle))
(use-package vterm-toggle
  :after vterm
  :general ("<f5>" 'vterm-toggle))

(use-package edit-server ; Edit with Emacs: edit web browser text boxes
  :init (edit-server-start)
  :hook ((edit-server-start . (lambda () (auto-fill-mode -1)))))

(use-package activity-watch-mode
  :diminish
  :config (global-activity-watch-mode))

;; TODO: notmuch again?


(provide 'init-ext)
