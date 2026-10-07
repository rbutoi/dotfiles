;;; elpaca-batch-update.el --- Update every Elpaca package from a batch Emacs  -*- lexical-binding: t; -*-

;;; Commentary:

;; Entry point for `.local/bin/elpaca_update.py', which runs
;;
;;   emacs --batch -l elpaca-batch-update.el -f elpaca-batch-update
;;
;; so that `elpaca-pull-all' runs in its own process and never blocks the
;; Emacs you are working in.  It is not meant to be required from `init.el'.
;;
;; `elpaca-pull-all' only touches packages that are *queued*, and the queue
;; is built by the `use-package' declarations in `early-init.el' and the
;; `init-*.el' modules.  A bare batch Emacs has an empty queue (and doesn't
;; read an init file at all), so this loads the real config to declare them.
;;
;; Running the real config headless is close to inert, which is what makes
;; this safe beside a live session:
;; - `use-package-always-defer' keeps each package's `:config' from running
;;   until the package is loaded, and nothing here loads them.
;; - `elpaca-after-init-hook' never fires, because the init files are loaded
;;   after startup has already finished (the queues are not "init" queues).
;; The exceptions are `:demand'/`:init' code with process-wide effects, which
;; is guarded with `noninteractive' in the config itself (`server-start',
;; `auto-dark-mode').  A new `:demand t' package that opens a port or takes a
;; socket needs the same guard.
;;
;; On the way out `kill-emacs-hook' is bypassed as a second line of defence:
;; `savehist', `recentf' and `save-place' write their state files from that
;; hook, and this process's copy is stale relative to the live Emacs.
;;
;; The queues are settled once before pulling, as a normal startup does.  That
;; is what resolves each package's dependencies into the queue; a pull-only
;; pass reports every dependency as "Missing".  It also installs anything
;; declared but absent from disk, as the next startup would anyway.
;;
;; Output goes to stdout as tab-separated records for the wrapper to parse:
;;   updated  <id>  <old-rev>  <new-rev>  <commit-count>
;;   failed   <id>  <last-logged-message>
;; Exit status: 0 all packages processed, 1 some failed, 2 timed out.

;;; Code:

(require 'cl-lib)

(defvar elpaca--event-log)
(declare-function elpaca--queued "elpaca")
(declare-function elpaca-process-queues "elpaca")
(declare-function elpaca-pull-all "elpaca")
(declare-function elpaca<-source-dir "elpaca")
(declare-function elpaca<-status "elpaca")
(declare-function elpaca-event<-id "elpaca")
(declare-function elpaca-event<-payload "elpaca")

(defvar elpaca-batch-update-timeout (* 30 60)
  "Seconds to wait for all packages to reach a terminal status.")

(defun elpaca-batch-update--head (dir)
  "Return the HEAD revision of the git repository at DIR, or nil."
  (when (and dir (file-directory-p dir))
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil "-C" dir "rev-parse" "HEAD"))
        (string-trim (buffer-string))))))

(defun elpaca-batch-update--commit-count (dir old new)
  "Return the number of commits in DIR between OLD and NEW."
  (with-temp-buffer
    (if (zerop (call-process "git" nil t nil "-C" dir "rev-list" "--count"
                             (concat old ".." new)))
        (string-trim (buffer-string))
      "?")))

(defun elpaca-batch-update--source-heads ()
  "Return an alist of (SOURCE-DIR ID . HEAD) for every queued package.
Packages sharing a source repository (monorepos) appear once."
  (let (heads)
    (cl-loop for (id . e) in (elpaca--queued)
             for dir = (elpaca<-source-dir e)
             unless (or (null dir) (assoc dir heads))
             do (push (cons dir (cons id (elpaca-batch-update--head dir))) heads))
    (nreverse heads)))

(defun elpaca-batch-update--pending-p ()
  "Non-nil while any queued package has not finished or failed."
  (cl-loop for (_ . e) in (elpaca--queued)
           thereis (not (memq (elpaca<-status e) '(finished failed)))))

(defun elpaca-batch-update--failure-reason (id)
  "Return the most recent logged message for package ID, on one line."
  (let ((ev (cl-find id elpaca--event-log :key #'elpaca-event<-id)))
    (replace-regexp-in-string
     "[\t\n]+" " "
     (truncate-string-to-width
      (format "%s" (or (plist-get (and ev (elpaca-event<-payload ev)) :info) "unknown"))
      200))))

(defun elpaca-batch-update--exit (code)
  "Exit with CODE without running `kill-emacs-hook'."
  (let ((kill-emacs-hook nil))
    (kill-emacs code)))

(defun elpaca-batch-update--wait (deadline)
  "Block until every queued package is finished or failed, or exit on DEADLINE."
  (while (and (elpaca-batch-update--pending-p) (< (float-time) deadline))
    (sleep-for 0.5))
  (when (elpaca-batch-update--pending-p)
    (message "elpaca-batch-update: timed out after %ds" elpaca-batch-update-timeout)
    (elpaca-batch-update--exit 2)))

(defun elpaca-batch-update ()
  "Pull and rebuild every queued Elpaca package, report, and exit."
  (load (locate-user-emacs-file "early-init.el") nil t)
  (load (locate-user-emacs-file "init.el") nil t)
  (let ((deadline (+ (float-time) elpaca-batch-update-timeout))
        before failed)
    (message "elpaca-batch-update: %d packages declared" (length (elpaca--queued)))
    (elpaca-process-queues)
    (elpaca-batch-update--wait deadline)
    (setq before (elpaca-batch-update--source-heads))
    (message "elpaca-batch-update: updating %d packages" (length (elpaca--queued)))
    (elpaca-pull-all t)
    (elpaca-batch-update--wait deadline)
    (cl-loop for (dir id . old) in before
             for new = (elpaca-batch-update--head dir)
             when (and old new (not (equal old new)))
             do (princ (format "updated\t%s\t%s\t%s\t%s\n" id
                               (substring old 0 8) (substring new 0 8)
                               (elpaca-batch-update--commit-count dir old new))))
    (cl-loop for (id . e) in (elpaca--queued)
             when (eq (elpaca<-status e) 'failed)
             do (push id failed)
             (princ (format "failed\t%s\t%s\n" id (elpaca-batch-update--failure-reason id))))
    (elpaca-batch-update--exit (if failed 1 0))))

(provide 'elpaca-batch-update)
;;; elpaca-batch-update.el ends here
