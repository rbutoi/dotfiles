;; -*- lexical-binding: t; -*-
;; config-fns.el: longer functions copied in externally for workarounds, or
;; internally and tweaked. intentionally only definitions without any
;; hook-adding or functionality-changing.

;;;; lisp util
(defun add-list-to-list (dst src) ; https://emacs.stackexchange.com/a/68048/26271
  "Similar to `add-to-list', but accepts a list as 2nd argument"
  (set dst
       (append (eval dst) src)))

;;;; UI
;; https://tecosaur.github.io/emacs-config/#theme-modeline
(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the
modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                 '(coding-category-undecided coding-category-utf-8))
                           (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                t)))

;;;; file ops
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

;;;; editing: toggling commenting
(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (progn
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
      (forward-line))))

;;;; better C/M-w
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; https://github.com/leoliu/easy-kill/issues/37#issuecomment-1131090505
(defvar my-easy-kill-map nil)
(when (functionp 'which-key--show-keymap)
  ;; 简化which-key提示
  (with-eval-after-load 'which-key
    ;; 去掉easy-kill-前辍
    ;; (push '((nil . "easy-kill-digit-argument") . (nil . "")) which-key-replacement-alist)
    (push '((nil . "easy-kill-") . (nil . "")) which-key-replacement-alist)

    ;; 额外居然都显示easy-kill-thing，这里替换它们的显示
    ;; easy-kill-help 可以显示所有功能
    ;; (push '(("s" . "easy-kill-thing") . (nil . "symbol")) which-key-replacement-alist)
    (cl-dolist (one easy-kill-alist)
      (push `((,(regexp-quote (char-to-string (car one))) . "easy-kill-thing") . (nil . ,(symbol-name (nth 1 one)))) which-key-replacement-alist)
      )
    )

  (defadvice easy-kill-activate-keymap (before my-easy-kill-activate-keymap activate)
    (unless my-easy-kill-map
      (let ((easy-kill-base-map easy-kill-base-map))
        ;; remove number keys
        (cl-loop for i from 0 to 9
                 do (define-key easy-kill-base-map (number-to-string i) nil))
        (setq my-easy-kill-map (easy-kill-map))
        ))
    (which-key--show-keymap "keymap" my-easy-kill-map nil nil 'no-paging)
    )
  (defun my-set-transient-map-exit()
    (which-key--hide-popup))
  (defadvice set-transient-map (before my-set-transient-map activate)
    (let ((map (ad-get-arg 0)))
      ;; 判断是否是easy-kill的keymap
      (when (eq (lookup-key map "?") 'easy-kill-help)
        (ad-set-arg 2 'my-set-transient-map-exit)
        )))
  )

;;;; vterm / compile
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

;; run in vterm
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

;;;; completion: corfu, consult
;; https://github.com/minad/corfu#transfer-completion-to-the-minibuffer
(defun corfu-move-to-minibuffer ()
  (interactive)
  (let ((completion-extra-properties corfu--extra)
        completion-cycle-threshold completion-cycling)
    (apply #'consult-completion-in-region completion-in-region--data)))

;; adapted from https://github.com/minad/consult/wiki#find-files-using-fd
(defvar consult--fd-command nil)
(defun consult--fd-builder (input)
  (unless consult--fd-command
    (setq consult--fd-command
          (if (eq 0 (call-process-shell-command "fdfind"))
              "fdfind"
            "fd")))
  (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
               (`(,re . ,hl) (funcall consult--regexp-compiler
                                      arg 'extended t)))
    (when re
      (list :command (append
                      (list consult--fd-command
                            "--color=never" "--full-path"
                            "--hidden" "--follow" ; get this as well
                            (consult--join-regexps re 'extended))
                      opts)
            :highlight hl))))

(defun consult-fd (&optional dir initial)
  "Search for files in DIR using fd matching input regexp given INITIAL input."
  (interactive "P")
  (let* ((prompt-dir (consult--directory-prompt "fd" dir))
         (default-directory (cdr prompt-dir)))
    (find-file (consult--find (car prompt-dir) #'consult--fd-builder initial))))

;;;; slow terminal toggling
;; theme toggling, TODO: un-doom if needed
;; (defun my/disable-save-theme ()
;;   "Disable the theme, and save it to re-enable."
;;   (interactive)
;;   (disable-theme (car custom-enabled-themes))
;;   (setq my/old-doom-theme doom-theme
;;         doom-theme        nil))

;; (defun my/enable-saved-theme ()
;;   (interactive)
;;   (setq doom-theme my/old-doom-theme)
;;   (load-theme doom-theme t nil)
;;   (doom/reload-theme))

;; (defun my/slow-terminal ()
;;   "For when the terminal is very slow.

;; Like ChromeOS's hterm."
;;   (interactive)
;;   (my/disable-save-theme)
;;   (setq my/old-scroll-conservatively scroll-conservatively
;;         scroll-conservatively        0))

;; (defun my/fast-terminal ()
;;   (interactive)
;;   (my/enable-saved-theme)
;;   (setq scroll-conservatively my/old-scroll-conservatively))

;;;; buffer killing
(defun kill-other-buffers ()       ; https://stackoverflow.com/a/3417473/3919508
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (cl-remove-if-not 'buffer-file-name (buffer-list)))))

(defun my/kill-this-buffer ()           ; usually what is desired
  "Kill the current buffer."
  (interactive) (kill-buffer nil))
(defun my/kill-all-buffers ()
  "Close all buffers."
  (interactive)
  (delete-other-windows)
  (save-some-buffers)
  (let ((kill-buffer-query-functions '()))
    (mapc 'kill-buffer (buffer-list)))
  (message "All buffers closed."))

;;;; epilogue
;; Local Variables:
;; eval: (outshine-mode)
;; End:
