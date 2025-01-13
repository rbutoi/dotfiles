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

(when (executable-find "gls") (setopt insert-directory-program "gls")) ; macOS

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

;; forge		failed	magit installed version (4 2 0) lower than min required 4.2.1
;; https://github.com/magit/forge/discussions/738
;; (use-package forge)

(use-package git-gutter
  :diminish
  :init (global-git-gutter-mode)
  :custom
  (git-gutter:handled-backends '(git hg bzr svn))
  (git-gutter:update-interval 2)
  :general
  (:keymaps 'prog-mode-map
            "C-x v =" 'git-gutter:popup-hunk
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
