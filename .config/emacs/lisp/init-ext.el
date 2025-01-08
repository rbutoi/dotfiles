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

(when (executable-find "gls") (setopt insert-directory-program "gls"))

;; https://github.com/progfolio/elpaca/issues/343
;; (elpaca seq)                            ; comment after initial install
(elpaca (transient :wait t))
(use-package magit                      ; version control
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
  (magit-log-margin    '(t "%a %b %d %Y" magit-log-margin-width t 18))
  (magit-status-margin '(t "%a %b %d %Y" magit-log-margin-width t 18))
  (magit-pull-or-fetch t))

;; Package	Status	Info
;; forge		failed	magit installed version (4 2 0) lower than min required 4.2.1
;; (use-package forge)

(use-package git-gutter
  :diminish
  :init (global-git-gutter-mode)
  :custom
  (git-gutter:handled-backends '(git hg bzr svn))
  (git-gutter:update-interval 2)
  :general
  ("C-x v =" 'git-gutter:popup-hunk
   "C-x v s" 'git-gutter:stage-hunk
   "C-x v r" 'git-gutter:revert-hunk
   "C-x v p"
   (defun my/git-gutter:toggle-start-revision ()
     "Toggle git-gutter:start-revision between \"\" and \"HEAD^\"."
     (interactive)
     (if (string= git-gutter:start-revision "HEAD^")
         (git-gutter:set-start-revision "")
       (git-gutter:set-start-revision "HEAD^"))
     (message "Set git-gutter:start-revision to \"%s\"" git-gutter:start-revision)))
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
