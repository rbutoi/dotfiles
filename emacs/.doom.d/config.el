;; -*- lexical-binding: t; -*-
;;;; Prologue
(setq user-config-start-time (current-time))

;;; UI

;;;; Theme
(setq
 my-theme   'solarized-dark
 doom-theme my-theme
 doom-font  (font-spec :family "Fira Code" :size 14)
 doom-modeline-project-detection 'project)

;;;; Startup/shutdown
(setq
 confirm-kill-emacs nil
 initial-major-mode 'lisp-interaction-mode) ; undo Doom

;; Persist Emacs’ initial frame position, dimensions and/or full-screen state
;; across sessions (from Doom "Interesting snippets")
(when-let (dims (doom-store-get 'last-frame-size))
  (cl-destructuring-bind ((left . top) width height fullscreen) dims
    (setq initial-frame-alist
          (append initial-frame-alist
                  `((left . ,left)
                    (top . ,top)
                    (width . ,width)
                    (height . ,height)
                    (fullscreen . ,fullscreen))))))

(defun save-frame-dimensions ()
  (doom-store-put 'last-frame-size
                  (list (frame-position)
                        (frame-width)
                        (frame-height)
                        (frame-parameter nil 'fullscreen))))

(add-hook 'kill-emacs-hook #'save-frame-dimensions)

;;;; Buffers and windows
;; bind-keys* because M-binds are frequently rebound (magit, vterm for example)
(bind-keys*
 ("M-0"  . delete-window)
 ("M-1"  . delete-other-windows)
 ("M-2"  . split-window-below)
 ("M-3"  . split-window-right)
 ;; if not lambda, would trigger ace-window
 ("M-o"  . (lambda () (interactive) (other-window +1)))
 ("M-i"  . (lambda () (interactive) (other-window -1))))
(map!
 "M-l"     (cmd! (select-window (get-mru-window t t t)))
 "C-k"     'kill-current-buffer
 "C-S-k"   'doom/kill-other-buffers
 "C-S-M-k" 'doom/kill-all-buffers
 "C-x C-M-k" 'doom/kill-all-buffers)

;; might fix query-replace somehow??
;; https://github.com/syl20bnr/spacemacs/issues/10938#issuecomment-407291657
(setq frame-title-format nil)

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

(setq display-line-numbers-type nil) ; undo Doom

;;;; Popups
(map! "M-`" '+popup/toggle ; aliases tmm-menubar
      "M-~" 'tmm-menubar)  ; this aliases not-modified
(set-popup-rule! "^\\*Man.*\\*$" :ignore t)
(set-popup-rule! "^vterm.*$" :ignore t)

;;;; Ivy / counsel
(map! "C-c C-r" 'ivy-resume
      "C-x m"   'counsel-M-x
      "C-x C-m" 'counsel-M-x
      "C-x C-b" 'counsel-switch-buffer
      "C-x b"   'counsel-buffer-or-recentf
      "C-o"     'counsel-semantic-or-imenu
      "C-M-s"   (cmd! (counsel-rg (thing-at-point 'symbol)))
      ;; doesn't show hidden files
      "C-x f"   (defun counsel-file-jump-ask-dir () (interactive)
                       (execute-extended-command t "counsel-file-jump"))
      "C-x M-f" 'counsel-file-jump-ask-dir
      "C-M-o"   'swiper-isearch-thing-at-point
      :map isearch-mode-map
      "C-o"     'swiper-from-isearch
      :map ivy-minibuffer-map
      "C-k"    'ivy-alt-done ; because C-j is used by tmux
      :map counsel-find-file-map
      "C-l"    'counsel-up-directory)
;;.. can be replaced by DEL/C-l, but . is still useful for e.g. dired here
(setq ivy-extra-directories '("."))

;;;; Defrepeater
(map! [remap doom/toggle-line-numbers] (defrepeater #'doom/toggle-line-numbers)
      [remap +word-wrap-mode]          (defrepeater #'+word-wrap-mode)
      [remap string-inflection-cycle]  (defrepeater #'string-inflection-cycle))

;;; Editing

;;;; Movement
(setq set-mark-command-repeat-pop t) ; can keep C-u C-SPC C-SPC C-SPC...
(map! "M-p" 'backward-paragraph
      "M-n" 'forward-paragraph)

(defun case-sensitive-query-replace ()
  (interactive)
  (let ((case-fold-search nil))
    (call-interactively 'query-replace)))

;; enable auto fill in text modes, and prog mode comments
(toggle-text-mode-auto-fill)
(add-hook! prog-mode 'auto-fill-mode)
(setq comment-auto-fill-only-comments t)

;;;; Revert file
(map! "C-c r" 'revert-buffer)
(global-auto-revert-mode)

(defun modi/revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be
reverted. They will be reverted though if they were modified
outside emacs. Buffers visiting files which do not exist any more
or are no longer readable will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))
(map! "C-c R" 'modi/revert-all-file-buffers)

;;;; comment-or-uncomment-line-or-region
(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (progn
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
      (forward-line))))
(map! "M-[ q" 'comment-or-uncomment-line-or-region
      "M-;"   'comment-or-uncomment-line-or-region)

;;;; Better C-w
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-ring-save (before slick-cut activate compile)
  "When called interactively with no active region, save a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;;;; Dired
(use-package dired-hide-dotfiles
  :bind (:map dired-mode-map ("." . dired-hide-dotfiles-mode)))

;;; Programming

;;;; Languages
;; Perl
(after! perl-mode
  (map! "C-c C-d" :map perl-mode-map 'cperl-perldoc))

;; Data/config
(add-hook! (yaml-mode conf-unix-mode conf-space-mode)
  (run-mode-hooks 'prog-mode-hook))

;; C/C++
(after! cc-mode
  (map! "C-c C-o" :map c-mode-base-map
        (cmd! (ff-find-other-file nil 'ignore-include))))
(add-hook! c++-mode (c-set-offset 'innamespace [0]))
(sp-local-pair 'c++-mode "<" ">" :when '(sp-point-after-word-p))
(add-hook! 'c-mode-common-hook ; formatting
  (fset 'c-indent-region 'clang-format-region))
; disable c-indent-line-or-region so completing can work
(map! :map c-mode-base-map "TAB" nil)

;; LaTeX
(setq TeX-auto-untabify t)

;; Rust
(use-package rustic
  :init
  (setq rustic-lsp-server 'rust-analyzer
        rustic-lsp-client 'eglot)
  (add-hook! rustic-mode (run-mode-hooks 'prog-mode-hook)))

;; Elisp: enable outshine to fold away parts of config
(use-package outshine
  :hook (emacs-lisp-mode . outshine-mode)
  :config (setq outshine-cycle-emulate-tab nil))

;;;; Completion
(setq tab-always-indent 'complete)
(after! company
  (unless WORK
    (setq company-dabbrev-downcase nil)
    (map! :map (global-map c-mode-base-map)
          "TAB"     'company-indent-or-complete-common
          "C-<tab>" '+company/dabbrev ;; low-tech alternative
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
(defun close-compile-window-if-successful (buffer string)
  " close a compilation window if succeeded without warnings "
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not
        (with-current-buffer buffer
          (search-forward "warning" nil t))))
      (run-with-timer 1 nil
                      (lambda (window) (quit-window nil window))
                      (get-buffer-window buffer))))
(add-hook 'compilation-finish-functions 'close-compile-window-if-successful)
(map! "S-<f7>" (cmd! (switch-to-buffer "*compilation*"))
      :map prog-mode-map
      "<f7>" 'compile
      "<f8>" 'recompile)
(setq compilation-message-face 'default)

;;;; Magit
(map! "C-x   g" 'magit-status
      "C-x C-g" 'magit-status)
(setq magit-log-auto-more t
      magit-log-margin '(t "%a %b %d %Y" magit-log-margin-width t 18))
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
(map! "C-c m" 'notmuch)

(after! notmuch
  (setq
   notmuch-poll-script
   "~/bin/poll_gmi.sh"
   +notmuch-sync-backend nil
   +notmuch-sync-command notmuch-poll-script
   sendmail-program "msmtp"
   message-sendmail-f-is-evil t
   message-sendmail-extra-arguments '("--read-envelope-from")

   ;; Wrap to 100 cols and disable colours for readable HTML mail.
   notmuch-wash-wrap-lines-length 100
   shr-width notmuch-wash-wrap-lines-length
   shr-use-colors nil
   notmuch-show-text/html-blocked-images nil ; enable images
   notmuch-message-headers-visible t ; CCs are important

   ;; I don't mind the full hello.
   notmuch-hello-sections '(notmuch-hello-insert-header
                            notmuch-hello-insert-saved-searches
                            notmuch-hello-insert-search
                            notmuch-hello-insert-recent-searches
                            notmuch-hello-insert-alltags
                            notmuch-hello-insert-footer)
   notmuch-show-all-tags-list t
   notmuch-show-logo t

   notmuch-saved-searches
   (append
    '((:key "f" :name "flagged"   :query "is:flagged")
      (:key "s" :name "sent"      :query "date:1M.. is:sent")
      (:key "d" :name "drafts"    :query "is:draft")
      (:key "a" :name "all"       :query "*"))
    (if WORK                                 ; limit time range for performance
        '((:key "j" :name "unified inbox"      :query "date:2w.. and is:inbox")
          (:key "i" :name "work inbox"         :query "date:2w.. and is:inbox and is:work")
          (:key "I" :name "personal inbox"     :query "date:2w.. and is:inbox and not is:work")
          (:key "u" :name "work unread"        :query "date:2w.. and is:inbox and is:unread and is:work")
          (:key "U" :name "personal unread"    :query "date:2w.. and is:inbox and is:unread and not is:work")
          (:key "m" :name "work important"     :query "date:2w.. and is:inbox and is:important and is:work")
          (:key "M" :name "personal important" :query "date:2w.. and is:inbox and is:important and not is:work")
          (:key "b" :name "work broadcast"     :query "date:2w.. and is:broadcast and is:work")
          (:key "B" :name "personal broadcast" :query "date:2w.. and is:broadcast and not is:work"))
      '((:key "i" :name "inbox"      :query "date:2w.. and is:inbox")
        (:key "u" :name "unread"     :query "date:2w.. and is:inbox and is:unread")
        (:key "m" :name "important"  :query "date:2w.. and is:inbox and is:important"))))

   notmuch-tag-formats
   (append '(("unread"    (propertize tag 'face 'notmuch-tag-unread))
             ("inbox"     "i")
             ("work"      nil)
             ("important" "im"))
           (list (list "personal" (if WORK "p" ""))))

   notmuch-unread-search-term
   (concat "is:unread and is:inbox"
           (if WORK " and is:work" "")))
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
  (map! :map notmuch-search-mode-map
        "w"     (cmd! (notmuch-search-filter-by-tag "work"))
        "W"     (cmd! (notmuch-search-filter-by-tag "personal"))
        "u"     (cmd! (notmuch-search-filter-by-tag "unread"))
        "m"     (cmd! (notmuch-search-filter-by-tag "important"))
        "M"     (cmd! (notmuch-search-filter-by-not-tag "important"))
        "M-m"   'notmuch-mua-new-mail ; to replace above
        "d"     (cmd! (notmuch-search-add-tag
                       '("+trash" "-inbox" "-unread"))
                      (notmuch-search-next-thread))
        "M-u"   (cmd! (notmuch-search-add-tag '("-unread"))
                      (notmuch-search-next-thread))
        "C-M-u" (cmd! (notmuch-search-tag-all '("-unread")))
        "f"     (cmd! (notmuch-search-add-tag '("+flagged")))
        :map notmuch-tree-mode-map
        "w"     (cmd! (notmuch-tree-filter-by-tag "work"))
        "W"     (cmd! (notmuch-tree-filter-by-tag "personal"))
        "u"     (cmd! (notmuch-tree-filter-by-tag "unread"))
        "i"     (cmd! (notmuch-tree-filter-by-tag "important"))
        "I"     (cmd! (notmuch-tree-filter-by-not-tag "important"))
        "d"     (cmd! (notmuch-tree-add-tag
                       '("+trash" "-inbox" "-unread"))
                      (notmuch-tree-next-matching-message))
        "M-u"   (cmd! (notmuch-tree-add-tag '("-unread"))
                      (notmuch-tree-next-message))
        "C-M-u" (cmd! (notmuch-tree-tag-thread '("-unread"))
                      (notmuch-tree-next-thread))
        :map (notmuch-hello-mode-map
              notmuch-search-mode-map
              notmuch-tree-mode-map
              notmuch-show-mode-map)
        "C-M-s" 'counsel-notmuch
        "G"     (cmd! (minibuffer-message "Syncing mail...")
                      (set-process-sentinel
                       (start-process-shell-command "notmuch update" nil
                                                    +notmuch-sync-command)
                       ;; refresh notmuch buffers if sync was successful
                       (lambda (_process event)
                         (when (string= event "finished\n")
                           (message "Synced mail.")
                           (notmuch-refresh-all-buffers)))))
        "Q"     (cmd! (dolist (buf (buffer-list))
                        (with-current-buffer buf
                          ;; can't get the list working
                          (when (or (derived-mode-p 'notmuch-search-mode)
                                    (derived-mode-p 'notmuch-tree-mode)
                                    (derived-mode-p 'notmuch-show-mode))
                            (kill-buffer)))))
        "D"     'notmuch-rm-deleted-tag
        )

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
    (hl-fill-column-mode -1)
    (visual-fill-column-mode +1))
  (add-to-list '+word-wrap-text-modes 'notmuch-message-mode))

;;;; Circe
(after! circe
  (setq circe-default-nick "radu242"
        circe-network-options
        '(("Freenode" :host "chat.freenode.net" :port (6667 . 6697)
           :tls t
           :nickserv-password (lambda (server) (password-store-get "freenode"))
           :channels (:after-auth "#emacs")
           ))))

;;;; vterm
(map! :map vterm-mode-map
      "C-c" 'vterm-send-C-c
      "<C-backspace>" (cmd! (vterm-send-key (kbd "C-w")))
      "C-x C-t" 'vterm-copy-mode)

;;;; Terminal support
;; Automatically toggle themed mode if in terminal or not
(defun themed-if-window-system (frame)
  (if (window-system frame)
      (unless doom-theme
        (setq doom-theme my-theme)
        (load-theme my-theme t)
        (ivy-rich-mode t))
    (when doom-theme
      (setq doom-theme nil)
      (disable-theme my-theme)
      (ivy-rich-mode -1))))
(defun themed-if-window-system-this-frame ()
  (interactive) (themed-if-window-system (selected-frame)))
(add-hook 'after-make-frame-functions 'themed-if-window-system)
(add-hook! 'focus-in-hook 'themed-if-window-system-this-frame)
(themed-if-window-system-this-frame)

;; make window divider prettier in terminal
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

(map! ; for terminal availability
 "C-M-%"   'query-replace
 "M-%"     'query-replace-regexp
 "M-="     'er/expand-region
 "C-x M-k" 'doom/kill-other-buffers)

;; Print URL when opening browser when working over SSH, and to keep a log in
;; the messages buffer.
(define-advice browse-url (:before (url &rest args))
  (message "Opening %s in browser." url))

;;; Epilogue
;; Host-specific support
(when IS-MAC
  (menu-bar-mode -1) ; needed on macos?
  (when (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode))
  (mac-pseudo-daemon-mode)
  (setq dired-use-ls-dired nil))
(load (concat doom-private-dir "specific.el") 'noerror)

;; Server
(use-package server :config (unless (server-running-p) (server-start)))

;; Benchmark config
(setq user-config-runtime (float-time (time-subtract (current-time)
                                                     user-config-start-time)))
(add-hook! 'window-setup-hook :append
  (message "User config loaded in %.03fs" user-config-runtime) (message ""))

;; Local Variables:
;; byte-compile-warnings: (not free-vars interactive-only unresolved)
;; End:
