;;; config-fns.el --- longer functions  -*- lexical-binding: t; -*-
;; ...copied in externally for workarounds, or internally and
;; tweaked. intentionally only definitions without anyhook-adding or
;; functionality-changing.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stolen from https://github.com/doomemacs/doomemacs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro cmd! (&rest body)
  "Returns (lambda () (interactive) ,@body)
A factory for quickly producing interaction commands, particularly for keybinds
or aliases."
  (declare (doc-string 1))
  `(lambda (&rest _) (interactive) ,@body))

;;;

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
;;;
;; https://old.reddit.com/r/emacs/comments/1p7wm82/make_consultripgrep_grep_fd_completion_argument/
(defun consult--get-completion-options-from-help (exec)
  "Generate exec options table vai `exec' -h."
  (when (executable-find exec)
    (let* ((-h (shell-command-to-string (concat exec  " --help")))
           (-h-list (string-split -h "\\(\\.\\|:\\)\n"))
           (doc-left-pad 30))
      (mapcan (lambda (h)
                (let ((l (string-replace "\n" "" h)))
                  (when (string-match (rx-to-string
                                       '(: bol (* space)
                                           (group "-" (? "-") (+ (or alnum "-")))
                                           (? ", ") (? (group "-" (? "-") (+ (or alnum "-"))))
                                           (? "=" (+ (or "_" "-" alnum)))
                                           (+ space)
                                           (group (* any)) eol))
                                      l)
                    (let* ((short (match-string 1 l))
                           (long (match-string 2 l))
                           (doc (match-string 3 l))
                           (s-pad (- doc-left-pad (length short)))
                           (l-pad (when long (- doc-left-pad (length long))))
                           (s-doc (concat (make-string s-pad ?\s) doc))
                           (l-doc (when long (concat (make-string l-pad ?\s) doc))))
                      (if long
                          (list `(,short . ,s-doc)
                                `(,long . ,l-doc))
                        (list `(,short . ,s-doc)))))))
              -h-list))))

(defmacro def-consult-help (command exec)
  (let ((options-fun (intern (format "consult-%s-get-completion-options" exec)))
        (options-alist (intern (format "consult-%s-completion-options-alist" exec)))
        (annotion (intern (format "consult-%s-completion-annotation" exec)))
        (table (intern (format "consult-%s-completion-table" exec)))
        (capf (intern (format "consult-%s-completion-at-point" exec)))
        (adv (intern (format "consult-%s-with-completion-at-point" exec))))
    `(progn
       (defun ,options-fun ()
         "Generate options table vai -h."
         (consult--get-completion-options-from-help ,exec))

       (defcustom ,options-alist
         (,options-fun)
         ,(format "%s options alist." exec))

       (defun ,annotion (candidate)
         "Annotation for rg option."
         (cdr (assoc candidate ,options-alist)))

       (defun ,table ()
         "List all option for rg."
         (mapcar #'car ,options-alist))

       (defun ,capf ()
         "Completion option.
This is the function to be used for the hook `completion-at-point-functions'."
         (interactive)
         (let* ((bds (bounds-of-thing-at-point 'symbol))
                (start (car bds))
                (end (cdr bds)))
           (list start end (,table) :annotation-function #',annotion)))

       (defun ,adv (orign &rest args)
         (minibuffer-with-setup-hook
             (:append
              (lambda ()
                (add-hook 'completion-at-point-functions
                          #',capf nil t)))
           (apply orign args)))

       (advice-add ,command :around ',adv))))

(def-consult-help 'consult-ripgrep "rg")
(def-consult-help 'consult-fd "fd")
;;;

;;;;;;;;;;;
;; Magit ;;
;;;;;;;;;;;

;; http://endlessparentheses.com/create-github-prs-from-emacs-with-magit.html
(defun endless/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-push-remote)
                       "url"))
           (magit-get-current-branch))))

;;;;;;;;;;;;
;; fixing ;;
;;;;;;;;;;;;

;; https://genehack.blog/2024/02/fixing-an-emacs-typescript-ts-mode-problem/
;; this fixes a problem where v0.20.4 of this grammar blows up with emacs
(with-eval-after-load 'treesit-auto
  (defvar genehack/tsx-treesit-auto-recipe
    (make-treesit-auto-recipe
     :lang 'tsx
     :ts-mode 'tsx-ts-mode
     :remap '(typescript-tsx-mode)
     :requires 'typescript
     :url "https://github.com/tree-sitter/tree-sitter-typescript"
     :revision "v0.20.3"
     :source-dir "tsx/src"
     :ext "\\.tsx\\'")
    "Recipe for libtree-sitter-tsx.dylib")
  (add-to-list 'treesit-auto-recipe-list genehack/tsx-treesit-auto-recipe)

  (defvar genehack/typescript-treesit-auto-recipe
    (make-treesit-auto-recipe
     :lang 'typescript
     :ts-mode 'typescript-ts-mode
     :remap 'typescript-mode
     :requires 'tsx
     :url "https://github.com/tree-sitter/tree-sitter-typescript"
     :revision "v0.20.3"
     :source-dir "typescript/src"
     :ext "\\.ts\\'")
    "Recipe for libtree-sitter-typescript.dylib")
  (add-to-list 'treesit-auto-recipe-list genehack/typescript-treesit-auto-recipe))

;;;;;;;;;;;;;;
;; banished ;;
;;;;;;;;;;;;;;

(use-package treesit-auto
  :disabled                         ; definitely adds too much to file load time
  :custom (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package copilot                    ; GitHub Copilot
  :disabled                             ; hmm
  :diminish
  :ensure (:host github :repo "copilot-emacs/copilot.el")
  :custom
  (copilot-idle-delay 0.7)
  :hook (prog-mode . copilot-mode)
  :general
  (:keymaps 'copilot-completion-map
            "<tab>" 'copilot-accept-completion
            "M-f"   'copilot-accept-completion-by-word
            "C-M-n"   'copilot-next-completion
            "C-M-p"   'copilot-previous-completion
            "C-g"   'copilot-clear-overlay))

(use-package activity-watch-mode
  :disabled ; TODO: causing errors, re-eval
  :diminish
  :config (global-activity-watch-mode))

;;;


(provide 'init-fns)
