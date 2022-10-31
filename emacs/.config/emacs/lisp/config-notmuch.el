;; -*- lexical-binding: t; -*-

(use-package notmuch
  :defer 2
  :init
  (defun my/notmuch ()
    "Go straight to notmuch inbox."
    (interactive)
    (notmuch-search (concat "(is:inbox or is:sent) and date:"
                            (if my/work? "1w.. and is:work" "2w.."))))
  (defun my/notmuch-search-filter-by-not-tag (tag)
    (interactive)
    (notmuch-search (concat notmuch-search-query-string " and not is:" tag)))
  (defun my/notmuch-tree-filter-by-tag (tag)
    (interactive)
    (notmuch-tree (concat notmuch-tree-basic-query " and is:" tag)))
  (defun my/notmuch-tree-filter-by-not-tag (tag)
    (interactive)
    (notmuch-tree (concat notmuch-tree-basic-query " and not is:" tag)))
  (defun my/notmuch-rm-deleted-tag ()
    "Delete emails tagged 'deleted' from the filesystem."
    (interactive)
    (shell-command (concat
                    "notmuch search --output=files --format=text0 tag:deleted"
                    " | xargs -0 rm && notmuch new"))
    (notmuch-refresh-all-buffers))
  (defun my/toggle-notmuch-search-width ()
    "Toggle width of Notmuch search results."
    (interactive)
    (setq notmuch-search-result-format
          (if (eq notmuch-search-result-format
                  notmuch-search-result-format--narrow)
              notmuch-search-result-format--wide
            notmuch-search-result-format--narrow))
    (notmuch-search-refresh-view))

  (defun my/notmuch-show-browse-first-url ()
    (interactive)
    (setq urls-global-should-be-let-but-doesnt-work
          (notmuch-show--gather-urls))
    (if urls-global-should-be-let-but-doesnt-work
        (browse-url
         (nth 0 urls-global-should-be-let-but-doesnt-work))
      (message "No URLs found.")))

  (use-package pfuture)
  (defun my/notmuch-poll-async-done ()
    (message "Polling mail...done")
    (notmuch-refresh-this-buffer))
  (defun my/notmuch-poll-async ()
    "Don't block all of emacs while doing a mail fetch which could
take up to a minute (if stale)."
    (interactive)
    (message "Polling mail...")
    (let ((script (if notmuch-poll-script
                      (list notmuch-poll-script) '("notmuch" "new"))))
      (pfuture-callback script
        :on-success (my/notmuch-poll-async-done)
        :on-error (my/notmuch-poll-async-done))))
  (defun my/notmuch-poll-if-needed ()
    "Take note of out-of-date pulled mail (by more than 10 minutes)"
    (interactive)
    (let ((mail-sync-age
           (time-subtract
            (current-time)
            (file-attribute-modification-time
             (file-attributes "/tmp/mail_unread_count_personal")))))
      (unless (time-less-p mail-sync-age (seconds-to-time (* 10 60)))
        (message
         (format-seconds
          "notmuch mail is %d days, %h hours, %m minutes, %s seconds old! G to sync" mail-sync-age))
        (my/notmuch-poll-async))))

  (defun my/notmuch-kill-all-buffers ()
    "Kill all notmuch buffers."
    (interactive)
    (dolist (buf (buffer-list))
      (when (derived-mode-p 'notmuch-search-mode 'notmuch-tree-mode 'notmuch-show-mode)
        (kill-buffer buf))))
  (defun my/notmuch-open-in-gmail ()
    "Open in Gmail web view."
    (interactive)
    (browse-url
     (concat "https://mail.google.com"
             (if (string-match-p "broadcast" (buffer-name))
                 "/mail/u/0/#label/broadcast" ""))))
  (setq
   ;; laptop/muchsync needs an external script for proper ssh creds and since
   ;; notmuch's call-process wrapper doesn't pass any args
   notmuch-poll-script (when my/laptop? "notmuch_new_systemd.sh")

   sendmail-program "msmtp"
   message-sendmail-f-is-evil t
   message-sendmail-extra-arguments '("--read-envelope-from")

   ;; Wrap to 100 cols and disable colours for readable HTML mail.
   notmuch-wash-wrap-lines-length 100
   shr-width notmuch-wash-wrap-lines-length
   shr-use-colors nil
   notmuch-show-text/html-blocked-images nil  ; enable images
   notmuch-message-headers-visible t  ; CCs are important
   ;; Use it by default since it's more readable for email and not code:
   ;; https://notmuchmail.org/pipermail/notmuch/2013/016726.html.
   notmuch-multipart/alternative-discouraged '("text/plain")

   ;; notmuch-hello-sections '(notmuch-hello-insert-header
   ;;                          notmuch-hello-insert-saved-searches
   ;;                          notmuch-hello-insert-search
   ;;                          notmuch-hello-insert-recent-searches
   ;;                          notmuch-hello-insert-alltags
   ;;                          notmuch-hello-insert-footer)
   notmuch-show-all-tags-list t
   notmuch-show-logo t
   notmuch-show-indent-messages-width 2

   notmuch-search-oldest-first nil     ; no idea why this is the default
   notmuch-saved-searches
   '((:key "f" :name "flagged"   :query "is:flagged"        )
     (:key "s" :name "sent"      :query "date:1M.. is:sent" )
     (:key "d" :name "drafts"    :query "is:draft"          )
     (:key "a" :name "all"       :query "*"                 )
     ;; TODO: defer to specific:
     ;; (:key "i" :name "inbox"      :query "(is:inbox or is:sent) and date:2w.."                  )
     ;; ;; redundant
     ;; (:key "I" :name "inbox"      :query "(is:inbox or is:sent) and date:2w.."                  )
     ;; (:key "u" :name "unread"     :query "(is:inbox or is:sent) and date:2w.. and is:unread"    )
     ;; (:key "m" :name "important"  :query "(is:inbox or is:sent) and date:2w.. and is:important" )
     ;; (:key "b" :name "broadcast"  :query "is:broadcast date:2w.."                               )
     )
   notmuch-search-result-format--narrow
   '(("date"    . "%12s "     )
     ("count"   . "%-7s "     )
     ("authors" . "%-20s "    )
     ("subject" . "%-60.60s " )
     ("tags"    . "%s"        ))
   notmuch-search-result-format--wide
   (if my/workstation?
       '(("date"    . "%12s "       )
         ("count"   . "%-7s "       )
         ("authors" . "%-50s "      )
         ("subject" . "%-150.150s " )
         ("tags"    . "%s"          ))
     '(("date"    . "%12s "      )
       ("count"   . "%-7s "      )
       ("authors" . "%-35s "     )
       ("subject" . "%-122.122s ")
       ("tags"    . "%s"         )))
   notmuch-search-result-format notmuch-search-result-format--narrow



   notmuch-refresh-timer
   (when (executable-find "notmuch")
     (run-with-idle-timer (* 5 60) t 'notmuch-refresh-this-buffer))

   notmuch-unread-search-term
   (concat "is:unread and is:inbox" (if my/work? " and is:work" "")))
  :general
  ("C-c m" 'my/notmuch
   "C-c M" 'notmuch)
  (:keymaps
   'notmuch-search-mode-map
   "w"          (lambda () (interactive) (notmuch-search-filter-by-tag "work"))
   "W"          (lambda () (interactive) (notmuch-search-filter-by-tag "personal"))
   "u"          (lambda () (interactive) (notmuch-search-filter-by-tag "unread"))
   "i"          (lambda () (interactive) (notmuch-search-filter-by-tag "inbox"))
   "I"          (lambda () (interactive) (my/notmuch-search-filter-by-not-tag "inbox"))
   "m"          (lambda () (interactive) (notmuch-search-filter-by-tag "important"))
   "M"          (lambda () (interactive) (my/notmuch-search-filter-by-not-tag "important"))
   "M-m"        'notmuch-mua-new-mail
   "d"          (lambda () (interactive) (notmuch-search-add-tag
                                          '("+trash" "-inbox" "-unread"))
                  (notmuch-search-next-thread))
   "M-u"        (lambda () (interactive) (notmuch-search-add-tag '("-unread"))
                  (notmuch-search-next-thread))
   "C-M-u"      (lambda () (interactive) (notmuch-search-tag-all '("-unread")))
   "f"          (lambda () (interactive) (notmuch-search-add-tag '("+flagged")))
   "C-w"        'my/toggle-notmuch-search-width)
  (:keymaps
   'notmuch-tree-mode-map
   "w"          (lambda () (interactive) (my/notmuch-tree-filter-by-tag "work"))
   "W"          (lambda () (interactive) (my/notmuch-tree-filter-by-tag "personal"))
   "u"          (lambda () (interactive) (my/notmuch-tree-filter-by-tag "unread"))
   "i"          (lambda () (interactive) (my/notmuch-tree-filter-by-tag "inbox"))
   "I"          (lambda () (interactive) (my/notmuch-tree-filter-by-not-tag "inbox"))
   "m"          (lambda () (interactive) (my/notmuch-tree-filter-by-tag "important"))
   "M"          (lambda () (interactive) (my/notmuch-tree-filter-by-not-tag "important"))
   "M-m"        'notmuch-mua-new-mail
   "d"          (lambda () (interactive) (notmuch-tree-add-tag
                                          '("+trash" "-inbox" "-unread"))
                  (notmuch-tree-next-matching-message))
   "M-u"        (lambda () (interactive) (notmuch-tree-add-tag '("-unread"))
                  (notmuch-tree-next-message))
   "C-M-u"      (lambda () (interactive) (notmuch-tree-tag-thread '("-unread"))
                  (notmuch-tree-next-thread))
   "b"          (lambda () (interactive) (other-window 1) (my/notmuch-show-browse-first-url)))
  (:keymaps
   'notmuch-show-mode-map
   "<C-return>" 'browse-url-at-point
   "B"          'notmuch-show-resend-message
   "b"          'my/notmuch-show-browse-first-url
   "M-b"        'notmuch-show-browse-urls
   "F"          (lambda () (interactive) (notmuch-show-add-tag '("+flagged")))
   "C-M-u"      (lambda () (interactive) (notmuch-show-tag-all '("-unread"))))
  (:keymaps
   '(notmuch-hello-mode-map
     notmuch-search-mode-map
     notmuch-tree-mode-map
     notmuch-show-mode-map)
   "C-M-s" 'consult-notmuch
   "Q"     'my/notmuch-kill-all-buffers
   "D"     'my/notmuch-rm-deleted-tag
   "<f7>"  'my/notmuch-open-in-gmail
   "G"     'my/notmuch-poll-async)
  :config
  (use-package consult-notmuch)
  (setq ; TODO: :custom
   notmuch-tag-formats
   (append
    notmuch-tag-formats
    '(("inbox"      nil)
      ("broadcast"  nil)
      ("personal"   nil)
      ("work"       nil)
      ("list"       nil)
      ("primary"        (propertize tag  'face '(:foreground "tomato"        )))
      ("updates"        (propertize tag  'face '(:foreground "royal blue"    )))
      ("promotions"     (propertize tag  'face '(:foreground "magenta"       )))
      ("forums"         (propertize tag  'face '(:foreground "yellow"        )))
      ("bills"          (propertize tag  'face '(:foreground "deep sky blue" )))
      ("amazon"         (propertize tag  'face '(:foreground "orange red"    )))
      ("security-alert" (propertize tag  'face '(:foreground "brown"         )))
      ("important"      (propertize "im" 'face '(:foreground "brightgreen" ))))))
  (use-package notmuch-unread
    :if my/workstation?
    :straight (:host github :repo "rbutoi/notmuch-unread")
    :init (notmuch-unread-mode))

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
  (general-add-hook '(notmuch-search-mode-hook notmuch-tree-mode-hook)
                    'my/notmuch-poll-if-needed)

  (use-package visual-fill-column)
  ;; Send plaintext email as long lines, let receivers soft-wrap.
  (add-hook 'notmuch-message-mode-hook
            (lambda ()
              (auto-fill-mode -1)
              (setq-local fill-column 100)
              (visual-fill-column-mode +1)))

  (general-add-hook
   '(notmuch-search-mode-hook notmuch-tree-mode-hook notmuch-show-mode-hook)
   (lambda ()
     ;; (hl-line-mode 1)
     ;; (smartparens-mode -1) ??
     ;; Update waybar unread count faster than the 5 min poll
     (when my/wayland? (call-process-shell-command "pkill -RTMIN+9 waybar")))))

;; Include date in "on <date> <sender> wrote..." reply text
(with-eval-after-load 'message
  (setq message-citation-line-function 'message-insert-formatted-citation-line)
  (setq message-citation-line-format "\n\nOn %a, %d %b %Y at %H:%M, %f wrote:\n"))
