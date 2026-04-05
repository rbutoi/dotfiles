;;; init-vcs.el --- Version control  -*- lexical-binding: t; -*-

(setopt vc-follow-symlinks t)

(elpaca (transient))            ; https://github.com/progfolio/elpaca/issues/343
(use-package magit              ; git porcelain
  :hook (after-save . magit-after-save-refresh-status)
  :general
  ("C-x C-g"   'magit-status
   "C-x C-M-g" 'magit-list-repositories)
  (:keymaps 'magit-mode-map
            "M-m" 'magit-toggle-margin
            "@"   'endless/visit-pull-request-url)
  :custom
  (magit-repository-directories
   `(("~/dev" . 2)
     ("~/.dots/dotfiles" . 0)))
  (magit-log-auto-more t)
  (magit-pull-or-fetch t)
  :config
  (dotimes (i 4)
    (let ((n (1+ i)))
      (general-define-key
       :keymaps 'magit-mode-map         ; since M-{1, 2, 3} are overridden
       (format     "%d" n) (intern (format "magit-section-show-level-%d-all" n))
       (format "C-M-%d" n) (intern (format "magit-section-show-level-%d"     n)))))

  (setopt magit-margin-default-time-format "%a %Y-%m-%d %H:%M ") ; show weekday
  (setf (nth 3 magit-log-margin) nil)   ; don't show author in log margin by default
  (setcar magit-status-margin 't))      ; show margin initially
(use-package magit-delta                ; nicer diffs
  :after magit
  :hook magit-mode
  :config (setq-union magit-delta-delta-args '("--features" "magit-delta")))
(use-package git-modes)
(use-package forge)                     ; github issues and PRs in magit

(use-package vc-jj)                     ; jujutsu
(use-package majutsu :vc (:url "https://github.com/0WD0/majutsu" :rev :newest))

(use-package diff-hl                    ; margin diff markers
  :hook (elpaca-after-init . global-diff-hl-mode)
  :general
  ("C-x v a" 'diff-hl-amend-mode)
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t)
  (add-hook 'diff-hl-mode-hook
            (lambda ()
              (unless (display-graphic-p)
                (diff-hl-margin-local-mode)))))

(use-package git-link :general ("C-x v G" 'git-link)) ; github link at point


(provide 'init-vcs)
