;; init-emacs.el - Emacs-specific misc config  -*- lexical-binding: t; -*-

(setopt
 confirm-kill-processes     nil
 use-short-answers          t
 ;; use-package-compute-statistics t       ; for (use-package-report)
 initial-scratch-message    "")

(require 'server) (unless (server-running-p) (server-start)) ; emacs --daemon

(defun my/elpaca-auto-update ()
  "Update Elpaca packages if they haven't in a week."
  (when (let ((elpaca-repos-dir (expand-file-name "elpaca/repos/" user-emacs-directory))
              (one-week-ago (- (float-time) 10 ;; (* 7 24 60 60)
                               )))
          (and (file-directory-p elpaca-repos-dir)
               (not (cl-loop for entry in (directory-files elpaca-repos-dir t nil t)
                             thereis (> (float-time (file-attribute-modification-time
                                                     (file-attributes entry)))
                                        one-week-ago)))))
    (message "No Elpaca package updates in the last week, running update...")
    (elpaca-update-all t)))

(run-at-time 10 (* 24 60 60) #'my/elpaca-auto-update)

;;;;;;;;;;;;;;;;;;;;;;
;; extenal packages ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package f)
(use-package general)                   ; keybinds
(elpaca-wait)
(use-package no-littering               ; must be set before load path
  :init   (setopt no-littering-etc-directory (f-join user-emacs-directory "lisp/"))
  :custom (create-lockfiles nil)
  :config
  (no-littering-theme-backups)
  ;; load path setup
  (add-to-list 'load-path no-littering-etc-directory))
(elpaca-wait)


(provide 'init-emacs)
