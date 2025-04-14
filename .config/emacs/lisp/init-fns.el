;; config-fns.el: longer functions  -*- lexical-binding: t; -*-
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

(provide 'init-fns)
