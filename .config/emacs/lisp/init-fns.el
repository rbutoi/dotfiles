;; config-fns.el: longer functions  -*- lexical-binding: t; -*-
;; ...copied in externally for workarounds, or internally and
;; tweaked. intentionally only definitions without anyhook-adding or
;; functionality-changing.

;; TODO: copy needed things over

(defun add-list-to-list (dst src) ; https://emacs.stackexchange.com/a/68048/26271
  "Similar to `add-to-list', but accepts a list as 2nd argument"
  (set dst
       (append (eval dst) src)))

(defmacro with-system (type &rest body) ; (with-system gnu/linux (...))
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

;; better C/M-w, from 2009 blog:
;; https://emacs-fu.blogspot.com/2009/11/copying-lines-without-selecting-them.html
(defadvice kill-ring-save (before slick-cut activate compile)
  "When called interactively with no active region, save a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defun kill-other-buffers ()       ; https://stackoverflow.com/a/3417473/3919508
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (cl-remove-if-not 'buffer-file-name (buffer-list))))
 (message "Killed all other buffers."))

;;;;;;;;;;;;;
;; Consult ;;
;;;;;;;;;;;;;

(with-eval-after-load 'consult
  ;; https://github.com/minad/consult/wiki#toggle-preview-during-active-completion-session
  (defvar-local consult-toggle-preview-orig nil)

  (defun consult-toggle-preview ()
    "Command to enable/disable preview."
    (interactive)
    (if consult-toggle-preview-orig
	(setq consult--preview-function consult-toggle-preview-orig
              consult-toggle-preview-orig nil)
      (setq consult-toggle-preview-orig consult--preview-function
            consult--preview-function #'ignore)))

  (define-key vertico-map (kbd "M-P") #'consult-toggle-preview)

  ;; TODO: this prevents going up
  ;; ;; https://github.com/minad/consult/wiki#previewing-files-in-find-file
  ;; (setq read-file-name-function #'consult-find-file-with-preview)

  ;; (defun consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
  ;;   (interactive)
  ;;   (let ((default-directory (or dir default-directory))
  ;;         (minibuffer-completing-file-name t))
  ;;     (consult--read #'read-file-name-internal :state (consult--file-preview)
  ;;                    :prompt prompt
  ;;                    :initial initial
  ;;                    :require-match mustmatch
  ;;                    :predicate pred)))
  )                                     ; end consult

;;;;;;;;;;;
;; Magit ;;
;;;;;;;;;;;

;; ;; http://endlessparentheses.com/create-github-prs-from-emacs-with-magit.html
;; (defun endless/visit-pull-request-url ()
;;   "Visit the current branch's PR on Github."
;;   (interactive)
;;   (browse-url
;;    (format "https://github.com/%s/pull/new/%s"
;;            (replace-regexp-in-string
;;             "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
;;             (magit-get "remote"
;;                        (magit-get-push-remote)
;;                        "url"))
;;            (magit-get-current-branch))))

;; (eval-after-load 'magit
;;   '(define-key magit-mode-map "v"
;;      #'endless/visit-pull-request-url))

(provide 'init-fns)
