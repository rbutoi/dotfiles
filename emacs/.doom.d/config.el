;; -*- lexical-binding: t; -*-
;;; UI

;;;; Theme
;; TODO: maybe remove?
(setq user-config-start-time (current-time))

(setq
 my-theme   'solarized-dark
 doom-theme my-theme
 doom-font  (font-spec :family "Fira Code" :size 14))

;; Machinery to automatically toggle themed mode if in terminal or not
(defun themed-if-window-system (frame)
  (if (window-system frame)
      (unless doom-theme
        (setq doom-theme my-theme)
        (load-theme my-theme t))
    (when doom-theme
      (setq doom-theme nil)
      (disable-theme my-theme))))
(add-hook 'after-make-frame-functions 'themed-if-window-system)
(add-hook! 'focus-in-hook (themed-if-window-system (selected-frame)))

;; make window divider prettier in terminal
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

;;;; Buffers and windows
;; more convenient M-binds. * because M-binds are frequently rebound
(bind-keys*
 ("M-0"  . delete-window)
 ("M-1"  . delete-other-windows)
 ("M-2"  . split-window-below)
 ("M-3"  . split-window-right))
(map!
 "M-o"     (lambda! (other-window +1)) ; if not lambda, would trigger ace-window
 "M-i"     (lambda! (other-window -1))
 "M-l"     (lambda! (select-window (get-mru-window t t t)))
 "C-k"     'kill-current-buffer
 "C-S-k"   'doom/kill-other-buffers
 "C-S-M-k" 'doom/kill-all-buffers
 "C-x M-k" 'doom/kill-other-buffers ; for when in terminal
 "C-x C-M-k" 'doom/kill-all-buffers)

;; might fix query-replace somehow??
;; https://github.com/syl20bnr/spacemacs/issues/10938#issuecomment-407291657
(setq frame-title-format nil)

;; always keep windows balanced
(defadvice split-window-below (after restore-balanace-below activate)
  (balance-windows))
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
      "C-M-s"   (lambda! (counsel-rg (thing-at-point 'symbol)))
      ;; doesn't show hidden files
      "C-x f"   (defun counsel-file-jump-ask-dir () (interactive)
                       (execute-extended-command t "counsel-file-jump"))
      "C-x M-f" 'counsel-file-jump-ask-dir
      "C-s"     'swiper-isearch
      "C-r"     'swiper-isearch-backward
      "C-M-o"   'swiper-isearch-thing-at-point
      (:map ivy-minibuffer-map
        "C-k"   'ivy-alt-done) ; because C-j is used by tmux
      (:map counsel-find-file-map
        "C-l"   'counsel-up-directory))
;;.. can be replaced by DEL/C-l, but . is still useful for e.g. dired here
(setq ivy-extra-directories '("."))

;;;; Defrepeater
(map! [remap doom/toggle-line-numbers] (defrepeater #'doom/toggle-line-numbers)
      [remap +word-wrap-mode]          (defrepeater #'+word-wrap-mode)
      [remap string-inflection-cycle]  (defrepeater #'string-inflection-cycle))

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
      (:key "a" :name "all"       :query "*")
      (:key "l" :name "lists"     :query "date:1w.. is:list" :search-type tree))
    (if WORK                                 ; limit time range for performance
        '((:key "j" :name "unified inbox"      :query "date:2w.. and is:inbox")
          (:key "i" :name "work inbox"         :query "date:2w.. and is:inbox and is:work")
          (:key "I" :name "personal inbox"     :query "date:2w.. and is:inbox and is:personal")
          (:key "u" :name "unread work"        :query "date:2w.. and is:inbox and is:unread and is:work")
          (:key "U" :name "unread personal"    :query "date:2w.. and is:inbox and is:unread and is:personal")
          (:key "m" :name "important work"     :query "date:2w.. and is:inbox and is:important and is:work")
          (:key "M" :name "important personal" :query "date:2w.. and is:inbox and is:important and is:personal")
          (:key "b" :name "work broadcasts"    :query "date:2w.. and is:broadcast"))
      '((:key "i" :name "inbox"      :query "date:2w.. and is:inbox")
        (:key "u" :name "unread"     :query "date:2w.. and is:inbox and is:unread")
        (:key "m" :name "important"  :query "date:2w.. and is:inbox and is:important"))))

   notmuch-tag-formats
   (append '(("unread"    (propertize tag 'face 'notmuch-tag-unread))
             ("inbox"     "i")
             ("work"      nil)
             ("important" "im"))
           (list (list "personal" (if WORK "p" ""))))

   ;; Refresh notmuch every five minutes if it's active.
   notmuch-refresh-timer
   (run-with-idle-timer (* 5 60) t
                        (lambda () (when (string-match-p "notmuch" (buffer-name))
                                     (ignore-errors
                                       (notmuch-refresh-all-buffers)))))

   notmuch-unread-search-term
   (concat "is:unread and is:inbox"
           (if WORK " and is:work or (is:broadcast and is:unread)" "")))
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
  (map! :map notmuch-search-mode-map
        "i"     (lambda! (notmuch-search-filter-by-tag "work"))
        "I"     (lambda! (notmuch-search-filter-by-tag "personal"))
        "u"     (lambda! (notmuch-search-filter-by-tag "unread"))
        "m"     (lambda! (notmuch-search-filter-by-tag "important"))
        "M"     (lambda! (notmuch-search-filter-by-not-tag "important"))
        "d"     (lambda! (notmuch-search-add-tag
                          '("+trash" "-inbox" "-unread"))
                         (notmuch-search-next-thread))
        "M-u"   (lambda! (notmuch-search-add-tag '("-unread"))
                         (notmuch-search-next-thread))
        "C-M-u" (lambda! (notmuch-search-tag-all '("-unread")))
        :map notmuch-tree-mode-map
        "w"     (lambda! (notmuch-tree-filter-by-tag "work"))
        "W"     (lambda! (notmuch-tree-filter-by-tag "personal"))
        "u"     (lambda! (notmuch-tree-filter-by-tag "unread"))
        "i"     (lambda! (notmuch-tree-filter-by-tag "important"))
        "I"     (lambda! (notmuch-tree-filter-by-not-tag "important"))
        "d"     (lambda! (notmuch-tree-add-tag
                          '("+trash" "-inbox" "-unread"))
                         (notmuch-tree-next-matching-message))
        "M-u"   (lambda! (notmuch-tree-add-tag '("-unread"))
                         (notmuch-tree-next-message))
        "C-M-u" (lambda! (notmuch-tree-tag-thread '("-unread"))
                         (notmuch-tree-next-thread))
        :map (notmuch-hello-mode-map
              notmuch-search-mode-map
              notmuch-tree-mode-map
              notmuch-show-mode-map)
        "C-M-s" 'counsel-notmuch
        "G"     (lambda! (minibuffer-message "Syncing mail...")
                         (set-process-sentinel
                          (start-process-shell-command "notmuch update" nil
                                                       +notmuch-sync-command)
                          ;; refresh notmuch buffers if sync was successful
                          (lambda (_process event)
                            (when (string= event "finished\n")
                              (message "Synced mail.")
                              (notmuch-refresh-all-buffers)))))
        "Q"     (lambda! (doom-kill-matching-buffers
                          "^\\*notmuch-.*\\(search\\|tree\\)")))

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
  (add-to-list '+word-wrap-text-modes 'notmuch-message-mode)

  (defun notmuch-tree-show-message-in ()
    "Show the current message (in split-pane)."
    (interactive)
    (let ((id (notmuch-tree-get-message-id))
          (inhibit-read-only t)
          buffer)
      (when id
        ;; We close and reopen the window to kill off un-needed buffers
        ;; this might cause flickering but seems ok.
        (notmuch-tree-close-message-window)
        (setq notmuch-tree-message-window
              ;; (split-window-horizontally (/ (window-height) 4))
              (split-window-sensibly)) ; TODO: send this upstream
        (with-selected-window notmuch-tree-message-window
          ;; Since we are only displaying one message do not indent.
          (let ((notmuch-show-indent-messages-width 0)
                (notmuch-show-only-matching-messages t))
            (setq buffer (notmuch-show id))))
        ;; We need the `let' as notmuch-tree-message-window is buffer local.
        (let ((window notmuch-tree-message-window))
          (with-current-buffer buffer
            (setq notmuch-tree-message-window window)
            (add-hook 'kill-buffer-hook 'notmuch-tree-message-window-kill-hook)))
        (when notmuch-show-mark-read-tags
          (notmuch-tree-tag-update-display notmuch-show-mark-read-tags))
        (setq notmuch-tree-message-buffer buffer)))))

;;;; Misc / one-offs
(setq
 ;; Don't display line numbers by default.
 display-line-numbers-type nil
 ;; Don't confirm exit.
 confirm-kill-emacs nil)

;; Print URL when opening browser when working over SSH, and to keep a log in
;; the messages buffer.
(define-advice browse-url (:before (url &rest args))
  (message "Opening %s in browser." url))

(use-package highlight-thing
  :config
  ;; useful across buffers
  (setq highlight-thing-all-visible-buffers-p t
        highlight-thing-limit-to-region-in-large-buffers-p nil
        highlight-thing-narrow-region-lines 15
        highlight-thing-large-buffer-limit 5000))

(after! circe
  (setq circe-default-nick "radu242"
        circe-network-options
        '(("Freenode" :host "chat.freenode.net" :port (6667 . 6697)
           :tls t
           :nickserv-password (lambda (server) (password-store-get "freenode"))
           :channels (:after-auth "#emacs")
           :nickserv-mask "^NickServ!NickServ@services\\.$"
           :nickserv-identify-challenge "\C-b/msg\\s-NickServ\\s-identify\\s-<password>\C-b"
           :nickserv-identify-command "PRIVMSG NickServ :IDENTIFY {nick} {password}"
           :nickserv-identify-confirmation "^You are now identified for .*\\.$"
           :nickserv-ghost-command "PRIVMSG NickServ :GHOST {nick} {password}"
           :nickserv-ghost-confirmation "has been ghosted\\.$\\|is not online\\.$"
           ))))

;;; Editing

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

;;;; M-{n,p} for paragraph movement
(map! "M-p" 'backward-paragraph
      "M-n" 'forward-paragraph)

;;;; goto-chg
(use-package goto-chg
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))

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

;;;; Misc / one-offs
(use-package string-inflection
  :bind (:map prog-mode-map ("C-c C-u" . string-inflection-cycle)))

(use-package dired-hide-dotfiles
  :bind (:map dired-mode-map ("." . dired-hide-dotfiles-mode)))

;; for terminal availability
(map! "C-M-%" 'query-replace
      "M-%"   'query-replace-regexp ; prioritize for terminal availability
      "M-="   'er/expand-region)
(defun case-sensitive-query-replace ()
  (interactive)
  (let ((case-fold-search nil))
    (call-interactively 'query-replace)))

;; can keep C-u C-SPC C-SPC C-SPC...
(setq set-mark-command-repeat-pop t)

(add-hook! text-mode 'auto-fill-mode 'flyspell-mode)

;;; Programming

;;;; Languages
;; Perl
(after! perl-mode
  (map! "C-c C-d" :map perl-mode-map 'cperl-perldoc))

;; Assembler
(after! asm-mode
  (map! "TAB" :map asm-mode-map 'asm-indent-line))

;; Data/config
(add-hook! (yaml-mode conf-unix-mode conf-space-mode)
  (run-mode-hooks 'prog-mode-hook))

;; C/C++
(after! cc-mode
  (map! "C-c C-o" :map c-mode-base-map
        (lambda! (ff-find-other-file nil 'ignore-include))))
(add-hook! c++-mode (c-set-offset 'innamespace [0]))
(sp-local-pair 'c++-mode "<" ">" :when '(sp-point-after-word-p))
(add-hook! 'c-mode-common-hook ; formatting
  (fset 'c-indent-region 'clang-format-region))

;; LaTeX
(setq TeX-auto-untabify t)

;; Rust
(add-hook! rust-mode (run-mode-hooks 'prog-mode-hook))

;;;; Company
(map! "TAB"     'company-indent-or-complete-common
      "C-<tab>" 'dabbrev-expand ;; low-tech alternative
      "M-/"     'dabbrev-expand)
(setq tab-always-indent        'complete
      company-dabbrev-downcase nil)

;;;; Flycheck
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
(map! "S-<f7>" (lambda! (switch-to-buffer "*compilation*"))
      :map prog-mode
      "<f7>" 'compile
      "<f8>" 'recompile)
(setq compilation-message-face 'default)

;;;; Magit
(map! "C-x   g" 'magit-status
      "C-x C-g" 'magit-status)
(setq magit-log-auto-more t
      magit-log-margin '(t "%a %b %d %Y" magit-log-margin-width t 18))
(use-package keychain-environment :config (keychain-refresh-environment))

;;;; Misc / one-offs
(add-hook! prog-mode 'highlight-thing-mode 'which-function-mode)

;;; End

;;;; macos section?
(when IS-MAC
  (exec-path-from-shell-initialize)
  (menu-bar-mode -1) ; needed on macos?
  (when (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode)))

;;;; endend

(load (concat doom-private-dir "specific.el") 'noerror)

(use-package server :config (unless (server-running-p) (server-start)))

;; echo benchmarked time
(setq user-config-runtime (float-time (time-subtract (current-time)
                                                     user-config-start-time)))
(add-hook! 'window-setup-hook :append
  (message "User config loaded in %.03fs" user-config-runtime) (message ""))
