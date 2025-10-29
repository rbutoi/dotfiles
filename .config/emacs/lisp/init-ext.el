;; init-ext.el - External integrations  -*- lexical-binding: t; -*-

;; TODO: broken, https://mail.gnu.org/archive/html/bug-gnu-emacs/2024-07/msg00824.html
;; (url-handler-mode)                      ; open HTTP links in Emacs

(xterm-mouse-mode)                      ; mouse in terminal Emacs

;;;;;;;;;;;;;;;;;;;;;;
;; extenal packages ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (when (executable-find "gls") (setopt insert-directory-program "gls")))

(use-package man                        ; man(1)
  :ensure nil
  :custom
  (Man-width-max nil)
  (Man-notify-method 'aggressive)
  :general (:keymaps 'Man-mode-map "/" 'isearch-forward-word))

;; TODO: figure out a better place to run this

;; we want to override a `defmacro`: https://github.com/search?q=repo%3Aemacs-mirror%2Femacs+%22defmacro+Man-start-calling%22&type=code
;; that means you can't just re-define the function. it seems like the cleanest way is to just patch the whole man.el file??

;; patch man.el to let width work properly on macOS
(let ((get-file-contents-shell
       (if (file-exists-p
            (concat
             (file-name-sans-extension (locate-library "man")) ".el"))
           "cat %s" "gunzip %s.gz")))
  (let ((upstream-el-file-md5
         (string-trim
          (shell-command-to-string
           (concat
            get-file-contents-shell " | md5sum | sed 's/\s.*//'" )))))
    (let ((my-file (format "/tmp/my-man-from-%s.el" upstream-el-file-md5))) ; TODO: dir
      (unless (file-exists-p my-file)
        (write-region
         (string-replace
          "(setenv \"COLUMNS\" (number-to-string Man-columns))"
          "(setenv \"COLUMNS\" (number-to-string Man-columns))
      (setenv \"MANWIDTH\" (number-to-string Man-columns))"
          (shell-command-to-string get-file-contents-shell))
         nil
         my-file))

      (load-file my-file)
      )))

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
  (magit-repository-directories `(("~/.dots/dotfiles" . 0)
                                  ("~/dev"            . 2)
                                  ("~/oss"            . 1)))
  (magit-log-auto-more t)
  (magit-pull-or-fetch t))
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
(use-package forge)

(use-package diff-hl                    ; margin diff markers
  :defer 1
  :general
  (:keymaps 'diff-hl-mode-map
            "C-x v [" (defrepeater 'diff-hl-previous-hunk)
            "C-x v ]" (defrepeater 'diff-hl-next-hunk))
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

(use-package verb
  :general
  (:keymaps 'org-mode-map
            "C-c C-r" verb-command-map))

(use-package google-this)               ; Google word at point

;; external servers

(use-package atomic-chrome              ; edit Chrome text fields in Emacs
  :config
  (atomic-chrome-start-server))


(use-package activity-watch-mode
  :disabled                             ; TODO: causing errors, re-eval
  :diminish
  :config (global-activity-watch-mode))

(provide 'init-ext)
