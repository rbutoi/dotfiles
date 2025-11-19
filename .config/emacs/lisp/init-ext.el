;; init-ext.el - External integrations  -*- lexical-binding: t; -*-

(xterm-mouse-mode)                      ; mouse in terminal Emacs

;;;;;;;;;;;;;;;;;;;;;;
;; extenal packages ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (setenv "RIPGREP_CONFIG_PATH"
          (substitute-env-vars "$HOME/.config/ripgreprc")))

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
  :diminish
  :after magit
  :hook (magit-mode . magit-delta-mode)
  :custom (magit-delta-delta-args
           '(;; defaults
             "--max-line-distance" "0.6" "--true-color" "always" "--color-only"
             ;; magit doesn't like line numbers: https://github.com/dandavison/magit-delta/issues/13
             "--features" "magit-delta")))
(use-package git-modes)
(use-package forge :defer 5)

(use-package diff-hl                    ; margin diff markers
  :defer 1
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t)
  (add-hook 'diff-hl-mode-hook
            (lambda ()
              (unless (display-graphic-p)
                (diff-hl-margin-local-mode)))))

(use-package git-link :general ("C-x v G" 'git-link)) ; github link at point

(use-package vterm                      ; terminal
  :defer 1
  :custom
  (vterm-always-compile-module t))
(use-package vterm-toggle
  :after vterm
  :general
  ("<f5>" 'vterm-toggle)
  (:keymaps 'vterm-mode-map "<f5>" 'vterm-toggle))

(use-package verb
  :defer 5
  :general
  (:keymaps 'org-mode-map
            "C-c C-r" verb-command-map))

(use-package google-this)               ; Google word at point

;; external servers

(use-package atomic-chrome              ; edit Chrome text fields in Emacs
  :config
  (atomic-chrome-start-server))

;;;;;;;;;;;;;;;;;
;; OS-specific ;;
;;;;;;;;;;;;;;;;;

(with-system darwin
  (setopt mac-option-modifier       'meta
          mac-command-modifier      'super
          insert-directory-program  "gls" ; gnu coreutils
          manual-program            "gman"))


(provide 'init-ext)
