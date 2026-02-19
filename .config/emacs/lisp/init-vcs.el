;;; init-vcs.el --- Version control  -*- lexical-binding: t; -*-

(setopt vc-follow-symlinks t)

(elpaca (transient))            ; https://github.com/progfolio/elpaca/issues/343
(use-package magit              ; version control
  :hook
  (after-save . magit-after-save-refresh-status)
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

  (setcar magit-status-margin 't))       ; show margin initially

(use-package magit-delta                ; nicer magit diffs
  :after magit
  :hook magit-mode
  :custom (magit-delta-delta-args
           '(;; defaults
             "--max-line-distance" "0.6" "--true-color" "always" "--color-only"
             ;; magit doesn't like line numbers: https://github.com/dandavison/magit-delta/issues/13
             "--features" "magit-delta")))
(use-package git-modes)
(use-package forge)

(use-package vc-jj)
(use-package majutsu :ensure (:host github :repo "0WD0/majutsu"))

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
