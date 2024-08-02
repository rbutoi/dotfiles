;; init-ext.el - External integrations  -*- lexical-binding: t; -*-

(xterm-mouse-mode)                      ; mouse in terminal Emacs

(use-package man                        ; man(1)
  :ensure nil
  :custom
  (Man-width-max nil)
  (Man-notify-method 'aggressive)
  :general (:keymaps 'Man-mode-map "/" 'isearch-forward-word))

(use-package dired-hide-dotfiles        ; file manager
  :general (:keymaps 'dired-mode-map "." 'dired-hide-dotfiles-mode))

;; magit needs newer versions of `seq` and `transient` apparently.
;; (use-package seq)                       ; elpaca warns about this
(use-package transient)
(use-package magit                      ; version control
  :hook
  (after-save . magit-after-save-refresh-status)
  :general
  ("C-x C-g"   'magit-status
   "C-x C-M-g" 'magit-list-repositories)
  :custom
  (magit-repository-directories `(("~/.dots/dotfiles" . 0)
                                  ("~/.dots/private-dots" . 0)
                                  ("~/dev" . 1)))
  (magit-log-auto-more t)
  (magit-log-margin '(t "%a %b %d %Y" magit-log-margin-width t 18)))

(use-package forge)

(use-package git-gutter
  :diminish
  :init (global-git-gutter-mode)
  :custom
  (git-gutter:handled-backends '(git hg bzr svn))
  (git-gutter:hide-gutter t)
  (git-gutter:update-interval 2)
  :general
  ("C-x v =" 'git-gutter:popup-hunk
   "C-x v s" 'git-gutter:stage-hunk
   "C-x v r" 'git-gutter:revert-hunk)
  :config
  (with-eval-after-load 'consult
    (add-list-to-list 'git-gutter:update-commands
                      '(switch-to-buffer consult-buffer)))
  (general-def                          ; in :general it warns
   "C-c p"   (defrepeater 'git-gutter:previous-hunk)
   "C-c n"   (defrepeater 'git-gutter:next-hunk)))

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
            "M-o"  (lambda () (interactive) (other-window +1))
            "M-i"  (lambda () (interactive) (other-window -1))
            "<f5>" 'vterm-toggle
            ;; ;; terminal binds
            ;; "C-M-]" 'query-replace-regexp
            ))
(use-package vterm-toggle
  :after vterm
  :general ("<f5>" 'vterm-toggle))

(use-package edit-server ; Edit with Emacs: edit web browser text boxes
  :init (edit-server-start)
  :hook ((edit-server-start . (lambda () (auto-fill-mode -1)))))

;; TODO: notmuch again?


(provide 'init-ext)
