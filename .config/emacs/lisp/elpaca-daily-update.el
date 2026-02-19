;;; elpaca-daily-update.el --- Prompted Elpaca updates -*- lexical-binding: t; -*-

;;; Code:

(require 'async)
(require 'elpaca)
(require 'no-littering)

(defvar elpaca-daily-update-interval (* 24 60 60)
  "Seconds between update checks (default 24 hours).")

(defvar elpaca-daily-update-threshold (* 7 24 60 60)
  "Seconds repo must be old to trigger prompt (default 7 days).")

(defvar elpaca-daily-update-state-file "elpaca-update-check.time"
  "Filename relative to `no-littering-var-directory' to store the last check timestamp.")

(defun elpaca-daily-update--state-path ()
  (no-littering-expand-var-file-name elpaca-daily-update-state-file))

(defun elpaca-daily-update--get-last-check ()
  "Return timestamp of last check or 0."
  (let ((file (elpaca-daily-update--state-path)))
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (string-to-number (buffer-string)))
      0)))

(defun elpaca-daily-update--save-check-time ()
  "Save current time to state file."
  (let ((file (elpaca-daily-update--state-path)))
    (unless (file-exists-p (file-name-directory file))
      (make-directory (file-name-directory file) t))
    (with-temp-file file
      (insert (number-to-string (float-time))))))

;;;###autoload
(defun elpaca-daily-update-check ()
  "Check repo age and prompt to update if older than threshold.
If called interactively, it skips the 24h timer check and reports status
even if repositories are up to date."
  (interactive)
  (let* ((verbose-p (called-interactively-p 'interactive))
         (diff (- (float-time) (elpaca-daily-update--get-last-check))))

    (when (or verbose-p (> diff elpaca-daily-update-interval))
      (elpaca-daily-update--save-check-time)

      (async-start
       `(lambda ()
          (let ((dir (expand-file-name "elpaca/repos" user-emacs-directory)))

            (if (file-directory-p dir)
                (string-to-number
                 (shell-command-to-string
                  (concat "fd --no-ignore -H '\\.git$' " (shell-quote-argument dir)
                          " -x git -C {//} log -1 --format=\"%at\" | sort | tail -1")))
              0)))
       (lambda (last-commit-ts)
         (let* ((age (- (float-time) last-commit-ts))
                (days (/ age 86400.0)))

           (cond
            ((> age elpaca-daily-update-threshold)
             (when (y-or-n-p (format "Elpaca: Repos are %.1f days old. Update all packages? " days))
               (elpaca-update-menus)
               (elpaca-wait)
               (elpaca-update-all t)
               (elpaca-log nil t)))
            (verbose-p
             (message "Elpaca: Up to date (%.1f days old)." days)))))))))

(defun elpaca-daily-update-init ()
  "Initialize the daily update timer."
  ;; If you keep Emacs open for days, it will trigger close to the 24h mark.
  (run-with-idle-timer 5 (* 60 60) #'elpaca-daily-update-check))

;; Hook into Elpaca's initialization
(add-hook 'elpaca-after-init-hook #'elpaca-daily-update-init)

(provide 'elpaca-daily-update)
;;; elpaca-daily-update.el ends here
