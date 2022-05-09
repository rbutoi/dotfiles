;; -*- lexical-binding: t; -*-
;; config-fns.el - longer functions copied in externally for workarounds, or
;; internally and tweaked.

;;;; Popups: have to copy and remove from doom's - Man removed
(set-popup-rules!
  '(("^\\*Completions" :ignore t)
    ("^\\*Local variables\\*$"
     :vslot -1 :slot 1 :size +popup-shrink-to-fit)
    ("^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)"
     :vslot -2 :size 0.3  :autosave t :quit t :ttl nil)
    ("^\\*\\(?:doom \\|Pp E\\)"  ; transient buffers (no interaction required)
     :vslot -3 :size +popup-shrink-to-fit :autosave t :select ignore :quit t :ttl 0)
    ("^\\*doom:"  ; editing buffers (interaction required)
     :vslot -4 :size 0.35 :autosave t :select t :modeline t :quit nil :ttl t)
    ("^\\*doom:\\(?:v?term\\|e?shell\\)-popup"  ; editing buffers (interaction required)
     :vslot -5 :size 0.35 :select t :modeline nil :quit nil :ttl nil)
    ; man removed
    ("^\\*Calc"
     :vslot -7 :side bottom :size 0.4 :select t :quit nil :ttl 0)
    ("^\\*Customize"
     :slot 2 :side right :size 0.5 :select t :quit nil)
    ("^ \\*undo-tree\\*"
     :slot 2 :side left :size 20 :select t :quit t)
    ;; `help-mode', `helpful-mode'
    ("^\\*\\([Hh]elp\\|Apropos\\)"
     :slot 2 :vslot -8 :size 0.35 :select t)
    ("^\\*eww\\*"  ; `eww' (and used by dash docsets)
     :vslot -11 :size 0.35 :select t)
    ("^\\*info\\*$"  ; `Info-mode'
     :slot 2 :vslot 2 :size 0.45 :select t)

    ; mine:
    ("^\\*Async Shell Command\\*$" :ttl 0)))

(after! counsel
  ;; https://github.com/abo-abo/swiper/issues/1333#issuecomment-436960474
  (defun counsel-find-file-fallback-command ()
    "Fallback to non-counsel version of current command."
    (interactive)
    (when (bound-and-true-p ivy-mode)
      (ivy-mode -1)
      (add-hook 'minibuffer-setup-hook
                'counsel-find-file-fallback-command--enable-ivy))
    (ivy-set-action
     (lambda (current-path)
       (let ((old-default-directory default-directory))
         (let ((i (length current-path)))
           (while (> i 0)
             (push (aref current-path (setq i (1- i))) unread-command-events)))
         (let ((default-directory "")) (call-interactively 'find-file))
         (setq default-directory old-default-directory))))
    (ivy-done))
  (defun counsel-find-file-fallback-command--enable-ivy ()
    (remove-hook 'minibuffer-setup-hook
                 'counsel-find-file-fallback-command--enable-ivy)
    (ivy-mode t)))

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

;; based on http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))

(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (progn
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
      (forward-line))))

;;;; Better C/M-w
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

(defun run-in-vterm-kill (process event)
  "A process sentinel. Kills PROCESS's buffer if it is live."
  (let ((b (process-buffer process)))
    (and (buffer-live-p b)
         (kill-buffer b))))

(defun run-in-vterm (command)
  "Execute string COMMAND in a new vterm.

Interactively, prompt for COMMAND with the current buffer's file
name supplied. When called from Dired, supply the name of the
file at point.

Like `async-shell-command`, but run in a vterm for full terminal features.

The new vterm buffer is named in the form `*foo bar.baz*`, the
command and its arguments in earmuffs.

When the command terminates, the shell remains open, but when the
shell exits, the buffer is killed."
  (interactive
   (list
    (let* ((f (cond (buffer-file-name)
                    ((eq major-mode 'dired-mode)
                     (dired-get-filename nil t))))
           (filename (concat " " (shell-quote-argument (and f (file-relative-name f))))))
      (read-shell-command "Terminal command: "
                          (cons filename 0)
                          (cons 'shell-command-history 1)
                          (list filename)))))
  (with-current-buffer (vterm (concat "*" command "*"))
    (set-process-sentinel vterm--process #'run-in-vterm-kill)
    (vterm-send-string command)
    (vterm-send-return)))

(defun disable-theme-slow-terminal ()
  "For when the terminal is very slow.

Like ChromeOS's hterm."
  (interactive)
  (disable-theme (car custom-enabled-themes))
  (setq old-doom-theme            doom-theme
        old-scroll-conservatively scroll-conservatively
        doom-theme                nil
        scroll-conservatively     0))

(defun enable-theme-fast-terminal ()
  (interactive)
  (setq doom-theme            old-doom-theme
        scroll-conservatively old-scroll-conservatively)
  (load-theme doom-theme t nil)
  (doom/reload-theme))
