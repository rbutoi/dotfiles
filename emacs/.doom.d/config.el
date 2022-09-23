;; -*- lexical-binding: t; -*-
;; config-el - Doom Emacs config

;;;; Prologue
(setq user-config-start-time (current-time))

;;; UI

;;;; Theme
(setq doom-theme 'doom-gruvbox
      doom-font  (font-spec :family "JetBrains Mono" :size 15))
(set-frame-parameter (selected-frame) 'alpha  '(90 . 85))
(add-to-list    'default-frame-alist '(alpha . (90 . 85)))

;; TODO: auto-diable on wayland/terminal-only

;;;; Startup
(setq
 confirm-kill-processes nil
 initial-major-mode 'lisp-interaction-mode)  ; undo Doom
(require 'server) (or (server-running-p) (server-start))  ; doesn't start when run standalone

;;;; Buffers and windows
(map!
 "M-l"       (cmd! (select-window (get-mru-window t t t)))
 "M-0"       'delete-window
 "M-1"       'delete-other-windows
 "M-2"       'split-window-below
 "M-3"       'split-window-right
 "M-o"       (cmd! (other-window +1))
 "M-i"       (cmd! (other-window -1))
 "C-S-k"     'doom/kill-other-buffers
 "C-x M-k"   'doom/kill-other-buffers
 "C-S-M-k"   'doom/kill-all-buffers
 "C-x C-M-k" 'doom/kill-all-buffers
 ;; occassionally useful (e.g. w3m)
 "C-c M-S"   'scroll-bar-mode
 ;; map overrides
 :map magit-mode-map  ;; using "1"-"3" shortcuts works the same as M-1 in magit
 "M-1"       'delete-other-windows
 "M-2"       'split-window-below
 "M-3"       'split-window-right
 )

;; keep windows balanced
(defadvice split-window-below (after restore-balanace-below activate)
  (unless (derived-mode-p 'notmuch-tree-mode)
    (balance-windows)))
(defadvice split-window-right (after restore-balance-right activate)
  (balance-windows))
(defadvice delete-window (after restore-balance activate)
  (balance-windows))

(use-package! zoom-window :bind (("C-x C-z" . zoom-window-zoom)))

(use-package! buffer-move
  :bind (("<C-S-up>" . buf-move-up)
         ("<C-S-down>" . buf-move-down)
         ("<C-S-left>" . buf-move-left)
         ("<C-S-right>" . buf-move-right)))

(remove-hook! text-mode #'display-line-numbers-mode)

(use-package! default-text-scale :config (default-text-scale-mode 1))

;;;; Popups
(after! rustic
  (set-popup-rule! "^\\*.*compilation.*\\*$" :ignore t))

(map!
 "<s-escape>" '+popup/toggle
 "M-`"        '+popup/toggle  ; aliases tmm-menubar
 "M-~"        'tmm-menubar)  ; this aliases not-modified

;;;; Ivy / counsel
(after! counsel
  (setq  ; .. can be replaced by DEL/C-l, but . is still useful for e.g. dired
   ivy-extra-directories '(".")
   ;; https://github.com/hlissner/doom-emacs/issues/3038#issuecomment-624165004
   counsel-rg-base-command
   "rg --max-columns 300 --with-filename --no-heading --line-number --color never --hidden %s 2>/dev/null || true"
   counsel-fd-command
   "fd --hidden --color never --follow --type file --type symlink --type directory ")

  (fset 'my/counsel-fd-file-jump-ask (cmd!! #'counsel-fd-file-jump '(4)))
  (fset 'my/counsel-rg-symbol-at-point (cmd! (counsel-rg (thing-at-point 'symbol))))
  (map! "C-c C-r"   'ivy-resume
        "C-x m"     'counsel-M-x
        "C-x C-m"   'counsel-M-x
        "C-x C-b"   'counsel-switch-buffer
        "C-x b"     'counsel-buffer-or-recentf
        "C-o"       'counsel-semantic-or-imenu
        "C-M-s"     'my/counsel-rg-symbol-at-point
        ;; doesn't show hidden files
        "C-x f"     'my/counsel-fd-file-jump-ask
        "C-x M-f"   'my/counsel-fd-file-jump-ask
        "C-x S-M-f" 'counsel-fd-file-jump
        "C-c M-c"   (defun my/find-dot-config () (interactive)
                      (counsel-fd-file-jump "" "~/.config/"))
        "C-M-o"     'swiper-isearch-thing-at-point
        :map isearch-mode-map
        "C-o"       'swiper-from-isearch
        :map ivy-minibuffer-map
        "C-k"       'ivy-alt-done       ; C-j is used by tmux
        "C-M-k"     'ivy-kill-line
        "C-M-i"     'ivy-insert-current ; M-i used to change windows
        :map counsel-find-file-map
        "C-l"       'counsel-up-directory
        "C-x C-f"   'counsel-find-file-fallback-command))


;;;; Defrepeater
(map! [remap doom/toggle-line-numbers] (defrepeater #'doom/toggle-line-numbers)
      [remap string-inflection-cycle]  (defrepeater #'string-inflection-cycle))

;;;; which-key
(use-package! which-key :config (setq which-key-show-docstrings t))

;;;; Word wrap
(map! "C-c C-w" 'toggle-truncate-lines)

;;;; Folding
(setq my/+folded-all nil)
(map! "C-\\"      '+fold/toggle
      "C-c C-\\"  (defun my/+fold-toggle-all ()
                    "Toggle all the folds in the buffer."
                    (interactive)
                    (if my/+folded-all (+fold/open-all) (+fold/close-all))
                    (setq my/+folded-all (not my/+folded-all))))

;;; Editing

(setq set-mark-command-repeat-pop t ; can keep C-u C-SPC C-SPC C-SPC...
      ; the original is    "[ \t]*\\([-–!|#%;>*·•‣⁃◦]+[ \t]*\\)*"
      adaptive-fill-regexp "[ \t]*\\([-–!|#%;>*·•‣⁃◦+]+[ \t]*\\)*"
      )
(toggle-text-mode-auto-fill)

(map! "M-g w"   'avy-goto-word-1      ; Avy binds, from its README.md
      "M-g C"   'avy-goto-char
      "C-M-]"   'query-replace-regexp ; terminal support for qrr
      "C-c r"   'revert-buffer        ; reverting
      "C-c R"   'modi/revert-all-file-buffers
      "M-;"     'comment-or-uncomment-line-or-region
      "M-[ q"   'comment-or-uncomment-line-or-region ; terminal support
      "C-c M-s" 'subword-mode
      "C-c a"   'align-regexp)
(defun my/case-sensitive-query-replace ()
  (interactive)
  (let ((case-fold-search nil)) (call-interactively 'query-replace)))
 (use-package! re-builder
  :config ; https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder
  (setq reb-re-syntax 'string))

;; fix doom :/ backspace is really slow in notmuch reply buffers when writing
;; replies inline
(advice-remove 'delete-backward-char #'+default--delete-backward-char-a)

;;;; Dired
(use-package! dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map ("." . dired-hide-dotfiles-mode)))

;;; Programming

;;;; Languages
(after! perl-mode (map! "C-c C-d" :map perl-mode-map 'cperl-perldoc))

(after! cc-mode
  (map! "C-c C-o" :map c-mode-base-map
        (cmd! (ff-find-other-file nil 'ignore-include))))
(add-hook! c++-mode (c-set-offset 'innamespace [0]))
(sp-local-pair 'c++-mode "<" ">" :when '(sp-point-after-word-p))
(add-hook! 'c-mode-common-hook  ; formatting
  (fset 'c-indent-region 'clang-format-region))
;; disable c-indent-line-or-region so completing can work
(map! :map c-mode-base-map "TAB" nil)

(after! tex (setq TeX-auto-untabify t))

(use-package! rustic
  :config
  (setq rustic-lsp-server 'rust-analyzer
        rustic-lsp-client 'eglot)
  (add-hook! rustic-mode (run-mode-hooks 'prog-mode-hook)))

(add-hook! after-save #'executable-make-buffer-file-executable-if-script-p)

(add-hook! conf-mode (run-mode-hooks 'prog-mode-hook))

;;;; tree-sitter
(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;;;; Completion
(setq tab-always-indent 'complete)
(after! company
  (unless WORK
    (setq company-dabbrev-downcase nil)
    (map! :map (global-map c-mode-base-map)
          "TAB"     'company-indent-or-complete-common
          "C-<tab>" '+company/dabbrev  ;; low-tech alternative
          "M-/"     '+company/dabbrev)))

;;;; Fly{make,check}
(after! flymake
  (map! :map flymake-mode-map
        "C-c C-e" #'flymake-show-diagnostics-buffer))
(after! flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;;; Diffing
(add-hook! diff-mode (read-only-mode t))
(map! "C-x C-v" 'vc-prefix-map)

;;;; Compiling
(add-hook 'compilation-finish-functions 'close-compile-window-if-successful)
(fset 'my/switch-to-compilation-buffer
      (cmd! (switch-to-buffer (buffer-name (car (doom-matching-buffers
                                                 "*compilation*"))))))
(map! "<f8>" 'my/switch-to-compilation-buffer
      :map prog-mode-map
      "<f7>"   'compile
      "S-<f7>" 'recompile)
(setq compilation-message-face 'default)
(add-hook! compilation-mode (setq truncate-lines nil) (hl-line-mode t))
(defun doom-apply-ansi-color-to-compilation-buffer-h ())  ;; another instance of Doom breaking things

;;;; Magit
(after! magit
  (setq magit-repository-directories
        `(("~/dotfiles" . 0)
          ("~/dotfiles-google" . 0)
          ("~/oss" . 1)
          ("~/.emacs.d" . 0))
        magit-log-auto-more t
        magit-log-margin '(t "%a %b %d %Y" magit-log-margin-width t 18))
  (map! "C-x   g"   'magit-status
        "C-x C-g"   'magit-status
        "C-x C-M-g" 'magit-list-repositories))
;; (use-package! keychain-environment :config (keychain-refresh-environment))

(use-package! git-gutter ;; git-gutter too
  :config
  (global-git-gutter-mode))

;;;; String-inflection
(use-package! string-inflection
  :config
  (map! :map (prog-mode-map c-mode-base-map sh-mode-map)
        "C-c C-u" 'string-inflection-cycle))

;;;; highlight-thing, which-function, rainbow-mode: visual programming things
(use-package! highlight-thing
  :config
  ;; useful across buffers
  (setq highlight-thing-all-visible-buffers-p t
        highlight-thing-limit-to-region-in-large-buffers-p nil
        highlight-thing-narrow-region-lines 15
        highlight-thing-large-buffer-limit 5000))
(add-hook! prog-mode 'highlight-thing-mode 'which-function-mode 'rainbow-mode)

;;; External

;;;; notmuch
(load (concat doom-private-dir "config-notmuch.el")) ; gets its own config file

;;;; vterm
(setq vterm-always-compile-module t
      vterm-max-scrollback 20000)
(map! :map vterm-mode-map
      "<C-backspace>" (cmd! (vterm-send-key (kbd "C-w"))))

;;;; Edit with Emacs: edit web browser text boxes
(edit-server-start)
(add-hook! edit-server-start (auto-fill-mode -1))

;;;; URLs
(url-handler-mode)  ; enable C-x C-f handler

;; Print URL when opening browser when working over SSH, and to keep a log in
;; the messages buffer.
(define-advice browse-url (:before (url &rest args))
  (message "Opening %s in browser." url)
  ;; name begins with *help to be a +popup
  (with-help-window "*helpful URL*" (format "URL to click: %s" (princ url))))

;;;; Man
(use-package! man
  :config
  (setq Man-width-max nil))  ; as wide as it goes

;;; Epilogue
(load (concat doom-private-dir "config-fns.el"))     ; useful function defininitons

;; Host-specific support
(when IS-MAC      (load (concat doom-private-dir "config-mac.el")      'noerror))
(when IS-CROSTINI (load (concat doom-private-dir "config-crostini.el") 'noerror))
(unless IS-CROSTINI  ; specific.el doesn't apply to crostini, which is just used to view email
  (load (concat doom-private-dir "specific.el") 'noerror))

;; Benchmark config
(add-hook! 'window-setup-hook :append
  (message "User config loaded in %.03fs"
           (float-time (time-subtract (current-time)
                                      user-config-start-time)))
  (message ""))  ; empty out minibuffer

;; Local Variables:
;; byte-compile-warnings: (not free-vars interactive-only unresolved)
;; End:
