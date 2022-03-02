;; -*- lexical-binding: t; -*-
;;;; Prologue
(setq user-config-start-time (current-time))

;;; UI

;;;; Theme
(setq doom-theme 'doom-gruvbox
      doom-font  (font-spec :family "JetBrains Mono" :size 14))
(set-frame-parameter (selected-frame) 'alpha  '(95 . 90))
(add-to-list    'default-frame-alist '(alpha . (95 . 90)))

;;;; Startup
(setq
 confirm-kill-processes nil
 initial-major-mode 'lisp-interaction-mode)  ; undo Doom

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
 "C-c M-S"   'scroll-bar-mode)

(setq doom-modeline-project-detection 'project)

;; keep windows balanced
(defadvice split-window-below (after restore-balanace-below activate)
  (unless (derived-mode-p 'notmuch-tree-mode)
    (balance-windows)))
(defadvice split-window-right (after restore-balance-right activate)
  (balance-windows))
(defadvice delete-window (after restore-balance activate)
  (balance-windows))

(use-package zoom-window :bind (("C-x C-z" . zoom-window-zoom)))

(use-package buffer-move
  :bind (("<C-S-up>" . buf-move-up)
         ("<C-S-down>" . buf-move-down)
         ("<C-S-left>" . buf-move-left)
         ("<C-S-right>" . buf-move-right)))

(remove-hook! text-mode #'display-line-numbers-mode)

;;;; Popups
(after! rustic
  (set-popup-rule! "^\\*.*compilation.*\\*$" :ignore t))

(map!
 "<s-escape>" '+popup/toggle
 "M-`"        '+popup/toggle  ; aliases tmm-menubar
 "M-~"        'tmm-menubar)  ; this aliases not-modified

;;;; Ivy / counsel
(after! counsel
  (map! "C-c C-r" 'ivy-resume
        "C-x m"   'counsel-M-x
        "C-x C-m" 'counsel-M-x
        "C-x C-b" 'counsel-switch-buffer
        "C-x b"   'counsel-buffer-or-recentf
        "C-o"     'counsel-semantic-or-imenu
        "C-M-s"   (cmd! (counsel-rg (thing-at-point 'symbol)))
        ;; doesn't show hidden files
        "C-x f"   'counsel-file-jump
        "C-x M-f" 'counsel-file-jump
        "C-M-o"   'swiper-isearch-thing-at-point
        :map isearch-mode-map
        "C-o"     'swiper-from-isearch
        :map ivy-minibuffer-map
        "C-k"     'ivy-alt-done  ; C-j is used by tmux
        "C-M-i"   'ivy-insert-current  ; M-i used to change windows
        :map counsel-find-file-map
        "C-l"     'counsel-up-directory
        "C-x C-f" 'counsel-find-file-fallback-command)
  (setq  ; .. can be replaced by DEL/C-l, but . is still useful for e.g. dired
   ivy-extra-directories '(".")
   ;; https://github.com/hlissner/doom-emacs/issues/3038#issuecomment-624165004
   counsel-rg-base-command
   "rg --max-columns 300 --with-filename --no-heading --line-number --color never --hidden %s 2>/dev/null || true"))

;;;; Defrepeater
(map! [remap doom/toggle-line-numbers] (defrepeater #'doom/toggle-line-numbers)
      [remap string-inflection-cycle]  (defrepeater #'string-inflection-cycle))

;;;; Word wrap
(map! "C-c C-w" 'toggle-truncate-lines)

;;;; Folding
(setq +fold/ed-all nil)
(map! "C-\\"      '+fold/toggle
      "C-c C-\\"  (defun +fold/my-toggle-all ()
                    "Toggle all the folds in the buffer."
                    (interactive)
                    (if +fold/ed-all (+fold/open-all) (+fold/close-all))
                    (setq +fold/ed-all (not +fold/ed-all))))

;;; Editing

;;;; Movement
(setq set-mark-command-repeat-pop t)  ; can keep C-u C-SPC C-SPC C-SPC...
(toggle-text-mode-auto-fill)
(map! "M-g w" 'avy-goto-word-1  ; Avy binds, from its README.md
      "M-g C" 'avy-goto-char)

;;;; Replacement
(defun case-sensitive-query-replace ()
  (interactive)
  (let ((case-fold-search nil)) (call-interactively 'query-replace)))
(map! "C-M-]" 'query-replace-regexp)  ; terminal support

;;;; Reverting
(map! "C-c r" 'revert-buffer
      "C-c R" 'modi/revert-all-file-buffers)

;;;; comment-or-uncomment-line-or-region
(map! "M-[ q" 'comment-or-uncomment-line-or-region
      "M-;"   'comment-or-uncomment-line-or-region)

;; fix doom :/ backspace is really slow in notmuch reply buffers when writing
;; replies inline
(advice-remove 'delete-backward-char #'+default--delete-backward-char-a)

;; paste when it doesn't work™
(map!
 "C-M-y" (cmd! (kill-new (string-trim (shell-command-to-string "wl-paste"))) (yank)))

(map! "C-c M-s" 'subword-mode)

;;;; Dired
(use-package dired-hide-dotfiles
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

(use-package rustic
  :config
  (setq rustic-lsp-server 'rust-analyzer
        rustic-lsp-client 'eglot)
  (add-hook! rustic-mode (run-mode-hooks 'prog-mode-hook)))

(add-hook! after-save #'executable-make-buffer-file-executable-if-script-p)

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
(map! "S-<f7>" (cmd! (switch-to-buffer (buffer-name (car (doom-matching-buffers
                                                          "*compilation*")))))
      :map prog-mode-map
      "<f7>" 'compile
      "<f8>" 'recompile)
(setq compilation-message-face 'default)
(add-hook! compilation-mode (setq truncate-lines nil) (hl-line-mode t))
(defun doom-apply-ansi-color-to-compilation-buffer-h ())  ;; another instance of Doom breaking things

;;;; Magit
(setq magit-repository-directories
      `(("~/dotfiles" . 0)
        ("~/dotfiles-google" . 0)
        ("~/oss" . 1)
        ("~/.emacs.d" . 0))
      magit-log-auto-more t
      magit-log-margin '(t "%a %b %d %Y" magit-log-margin-width t 18))
(map! "C-x   g"   'magit-status
      "C-x C-g"   'magit-status
      "C-x C-M-g" 'magit-list-repositories)
(use-package keychain-environment :config (keychain-refresh-environment))

;;;; String-inflection
(use-package string-inflection
  :config
  (map! :map (prog-mode-map c-mode-base-map) "C-c C-u" 'string-inflection-cycle))

;;;; highlight-thing, which-function
(use-package highlight-thing
  :config
  ;; useful across buffers
  (setq highlight-thing-all-visible-buffers-p t
        highlight-thing-limit-to-region-in-large-buffers-p nil
        highlight-thing-narrow-region-lines 15
        highlight-thing-large-buffer-limit 5000))
(add-hook! prog-mode 'highlight-thing-mode 'which-function-mode)

;;; External

;;;; notmuch
(map!
 "C-c m" (defun my-notmuch ()
           "Go straight to notmuch inbox."
           (interactive)
           (notmuch-search (concat "(is:inbox or is:sent) and date:"
                                   (if WORK "1w.. and is:work" "2w.."))))
 "C-c M" 'notmuch)
(after! notmuch
  (setq
   sendmail-program "msmtp"
   message-sendmail-f-is-evil t
   message-sendmail-extra-arguments '("--read-envelope-from")

   ;; Wrap to 100 cols and disable colours for readable HTML mail.
   notmuch-wash-wrap-lines-length 100
   shr-width notmuch-wash-wrap-lines-length
   shr-use-colors nil
   notmuch-show-text/html-blocked-images nil  ; enable images
   notmuch-message-headers-visible t  ; CCs are important
   ;; Use it by default since it's more readable for email and not code: https://notmuchmail.org/pipermail/notmuch/2013/016726.html.
   notmuch-multipart/alternative-discouraged '("text/plain")

   notmuch-hello-sections '(notmuch-hello-insert-header
                            notmuch-hello-insert-saved-searches
                            notmuch-hello-insert-search
                            notmuch-hello-insert-recent-searches
                            notmuch-hello-insert-alltags
                            notmuch-hello-insert-footer)
   notmuch-show-all-tags-list t
   notmuch-show-logo t
   notmuch-show-indent-messages-width 2

   notmuch-saved-searches
   (append
    '((:key "f" :name "flagged"   :query "is:flagged"        )
      (:key "s" :name "sent"      :query "date:1M.. is:sent" )
      (:key "d" :name "drafts"    :query "is:draft"          )
      (:key "a" :name "all"       :query "*"                 ))
    (if WORK
        '((:key "j" :name "unified inbox"      :query "(is:inbox or is:sent) and date:1w.."                                  )
          (:key "i" :name "work inbox"         :query "(is:inbox or is:sent) and date:1w.. and is:work"                      )
          (:key "I" :name "personal inbox"     :query "(is:inbox or is:sent) and date:2w.. and is:personal"                  )
          (:key "u" :name "work unread"        :query "(is:inbox or is:sent) and date:1w.. and is:unread and is:work"        )
          (:key "U" :name "personal unread"    :query "(is:inbox or is:sent) and date:2w.. and is:unread and is:personal"    )
          (:key "m" :name "work important"     :query "(is:inbox or is:sent) and date:1w.. and is:important and is:work"     )
          (:key "M" :name "personal important" :query "(is:inbox or is:sent) and date:2w.. and is:important and is:personal" )
          (:key "S" :name "work snippets"      :query "is:inbox              and date:2w.. and subject:'\[snippets\]'")
          (:key "b" :name "work broadcast"     :query "is:broadcast and not is:list and date:2w.. and is:work"  )
          (:key "B" :name "personal broadcast" :query "is:broadcast and date:2w.. and is:personal"              ))
      '((:key "i" :name "inbox"      :query "(is:inbox or is:sent) and date:2w.."                  )
        (:key "I" :name "inbox"      :query "(is:inbox or is:sent) and date:2w.."                  )  ; redundant
        (:key "u" :name "unread"     :query "(is:inbox or is:sent) and date:2w.. and is:unread"    )
        (:key "m" :name "important"  :query "(is:inbox or is:sent) and date:2w.. and is:important" )
        (:key "b" :name "broadcast"  :query "is:broadcast date:2w.."              ))))
   notmuch-search-result-format--narrow
   '(("date"    . "%12s "     )
     ("count"   . "%-7s "     )
     ("authors" . "%-20s "    )
     ("subject" . "%-60.60s " )
     ("tags"    . "%s"        ))
   notmuch-search-result-format--wide
   '(("date"    . "%12s "       )
     ("count"   . "%-7s "       )
     ("authors" . "%-50s "      )
     ("subject" . "%-120.120s " )
     ("tags"    . "%s"          ))
   notmuch-search-result-format notmuch-search-result-format--narrow

   notmuch-tag-formats
   (append notmuch-tag-formats
           '(("inbox"      nil)
             ("broadcast"  nil)
             ("personal"   nil)
             ("work"       nil)
             ("primary"    (propertize tag 'face '(:foreground "tomato")))
             ("updates"    (propertize tag 'face '(:foreground "royal blue")))
             ("promotions" (propertize tag 'face '(:foreground "magenta")))
             ("forums"     (propertize tag 'face '(:foreground "yellow")))
             ("bills"      (propertize tag 'face '(:foreground "deep sky blue")))
             ("amazon"     (propertize tag 'face '(:foreground "orange red")))
             ("security-alert" (propertize tag 'face '(:foreground "brown"))))
           (list (list "important" (if WORK "im" (propertize "im" 'face '(:foreground "green"))))))

   notmuch-refresh-timer (run-with-idle-timer (* 5 60) t 'notmuch-refresh-this-buffer)

   notmuch-unread-search-term (concat "is:unread and is:inbox" (if WORK " and is:work" "")))
  (notmuch-unread-mode)

  ;; Make search coloured like tree (why are they different?)
  (face-spec-set 'notmuch-search-date
                 '((t :inherit notmuch-tree-match-date-face)))
  (face-spec-set 'notmuch-search-matching-authors
                 '((t :inherit notmuch-tree-match-author-face)))
  ;; TODO: this one inexplicably doesn't work when the previous do:
  (face-spec-set 'notmuch-tree-match-tag-face
                 '((t :inherit notmuch-tag-face)))
  ;; Add some more visibility besides bold, which is invisible on non-Retina Mac
  ;; screens.
  (face-spec-set 'notmuch-search-unread-face
                 '((t :bold 't :underline t)))

  (defun notmuch-search-filter-by-not-tag (tag)
    (notmuch-search (concat notmuch-search-query-string " and not is:" tag)))
  (defun notmuch-tree-filter-by-tag (tag)
    (notmuch-tree (concat notmuch-tree-basic-query " and is:" tag)))
  (defun notmuch-tree-filter-by-not-tag (tag)
    (notmuch-tree (concat notmuch-tree-basic-query " and not is:" tag)))
  (defun notmuch-rm-deleted-tag ()
    "Delete emails tagged 'deleted' from the filesystem."
    (interactive)
    (shell-command (concat
                    "notmuch search --output=files --format=text0 tag:deleted"
                    " | xargs -0 rm && notmuch new"))
    (notmuch-refresh-all-buffers))
  (defun toggle-notmuch-search-width ()
    "Toggle width of Notmuch search results."
    (interactive)
    (setq notmuch-search-result-format
          (if (eq notmuch-search-result-format notmuch-search-result-format--narrow)
              notmuch-search-result-format--wide
            notmuch-search-result-format--narrow))
    (notmuch-search-refresh-view))
  (defun notmuch-show-browse-first-url ()
    (interactive)
    (setq urls-global-should-be-let-but-doesnt-work
          (notmuch-show--gather-urls))
    (if urls-global-should-be-let-but-doesnt-work
        (browse-url
         (nth 0 urls-global-should-be-let-but-doesnt-work))
      (message "No URLs found.")))
  (map! :map notmuch-search-mode-map
        "w"          (cmd! (notmuch-search-filter-by-tag "work"))
        "W"          (cmd! (notmuch-search-filter-by-tag "personal"))
        "u"          (cmd! (notmuch-search-filter-by-tag "unread"))
        "i"          (cmd! (notmuch-search-filter-by-tag "inbox"))
        "I"          (cmd! (notmuch-search-filter-by-not-tag "inbox"))
        "m"          (cmd! (notmuch-search-filter-by-tag "important"))
        "M"          (cmd! (notmuch-search-filter-by-not-tag "important"))
        "M-m"        'notmuch-mua-new-mail
        "d"          (cmd! (notmuch-search-add-tag
                            '("+trash" "-inbox" "-unread"))
                           (notmuch-search-next-thread))
        "M-u"        (cmd! (notmuch-search-add-tag '("-unread"))
                           (notmuch-search-next-thread))
        "C-M-u"      (cmd! (notmuch-search-tag-all '("-unread")))
        "f"          (cmd! (notmuch-search-add-tag '("+flagged")))
        "C-w"        'toggle-notmuch-search-width
        :map notmuch-tree-mode-map
        "w"          (cmd! (notmuch-tree-filter-by-tag "work"))
        "W"          (cmd! (notmuch-tree-filter-by-tag "personal"))
        "u"          (cmd! (notmuch-tree-filter-by-tag "unread"))
        "i"          (cmd! (notmuch-tree-filter-by-tag "inbox"))
        "I"          (cmd! (notmuch-tree-filter-by-not-tag "inbox"))
        "m"          (cmd! (notmuch-tree-filter-by-tag "important"))
        "M"          (cmd! (notmuch-tree-filter-by-not-tag "important"))
        "M-m"        'notmuch-mua-new-mail
        "d"          (cmd! (notmuch-tree-add-tag
                            '("+trash" "-inbox" "-unread"))
                           (notmuch-tree-next-matching-message))
        "M-u"        (cmd! (notmuch-tree-add-tag '("-unread"))
                           (notmuch-tree-next-message))
        "C-M-u"      (cmd! (notmuch-tree-tag-thread '("-unread"))
                           (notmuch-tree-next-thread))
        "b"          (cmd! (other-window 1) (notmuch-show-browse-first-url))
        :map notmuch-show-mode-map
        "<C-return>" 'browse-url-at-point
        "B"          'notmuch-show-resend-message
        "b"          'notmuch-show-browse-first-url
        "M-b"        'notmuch-show-browse-urls
        "F"          (cmd! (notmuch-show-add-tag '("+flagged")))
        "C-M-u"      (cmd! (notmuch-show-tag-all '("-unread")))
        :map (notmuch-hello-mode-map
              notmuch-search-mode-map
              notmuch-tree-mode-map
              notmuch-show-mode-map)
        "C-M-s" 'counsel-notmuch
        "Q"     (cmd! (dolist (buf (buffer-list))
                        (with-current-buffer buf
                          ;; can't get the list working
                          (when (or (derived-mode-p 'notmuch-search-mode)
                                    (derived-mode-p 'notmuch-tree-mode)
                                    (derived-mode-p 'notmuch-show-mode))
                            (kill-buffer)))))
        "D"     'notmuch-rm-deleted-tag
        "<f7>"  (cmd!
                 (browse-url
                  (concat "https://mail.google.com"
                          (if (string-match-p "broadcast" (buffer-name))
                              "/mail/u/0/#label/broadcast" "")))))
  ;; ignore doom's rule, prefer fullscreen
  (set-popup-rule! "^\\*notmuch-hello" :ignore t)

  (add-hook! (notmuch-search-mode notmuch-tree-mode)
    (defun notmuch-poll-if-needed ()
      "Poll for mail if the systemd timer hasn't fired yet (i.e.
just woke from suspend)."
      (unless (time-less-p  ;; if mtime > 10 minutes ago
               (time-subtract (current-time) (file-attribute-modification-time
                                              (file-attributes
                                               "~/.mail/personal/.lock")))
               (seconds-to-time (* 10 60)))
        (notmuch-poll-and-refresh-this-buffer))))

  ;; > modeline doesn't have much use in these modes
  ;; I beg to differ. Showing the current search term is useful, and removing
  ;; the modeline is disorienting.
  (remove-hook! '(notmuch-show-mode-hook
                  notmuch-tree-mode-hook
                  notmuch-search-mode-hook)
    #'hide-mode-line-mode)

  ;; Send plaintext email as long lines, let receivers soft-wrap.
  (add-hook! notmuch-message-mode
    (auto-fill-mode -1)
    (setq-local fill-column 100)
    (visual-fill-column-mode +1))
  ;; Update waybar unread count faster than the 5 min poll
  (add-hook! notmuch-search (call-process-shell-command "pkill -RTMIN+9 waybar"))
  (add-hook! (notmuch-search-mode notmuch-tree-mode notmuch-show-mode)
    (hl-line-mode 1) (smartparens-mode -1)))

;; Include date in "on <date> <sender> wrote..." reply text
(after! message
  (setq message-citation-line-function 'message-insert-formatted-citation-line)
  (setq message-citation-line-format "\n\nOn %a, %d %b %Y at %H:%M, %f wrote:\n"))

;;;; vterm
(setq vterm-always-compile-module t
      vterm-max-scrollback 20000)
(map! :map vterm-mode-map
      "<C-backspace>" (cmd! (vterm-send-key (kbd "C-w"))))

;;;; Edit with Emacs: edit web browser text boxes
(edit-server-start)
(add-hook! edit-server-start (auto-fill-mode -1) (visual-fill-column-mode))

;;;; Terminal support
;; (setq xterm-set-window-title nil)  ; seems to bug out

;;;; URLs
(url-handler-mode)  ; enable C-x C-f handler

;; Print URL when opening browser when working over SSH, and to keep a log in
;; the messages buffer.
(define-advice browse-url (:before (url &rest args))
  (message "Opening %s in browser." url))

;;;; Man
(setq Man-width-max nil)  ; as wide as it goes

;;; Epilogue
(server-start)  ; doesn't start when run standalone
(load (concat doom-private-dir "config-fns.el"))

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
