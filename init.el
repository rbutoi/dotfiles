(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face (quote default))
 '(custom-safe-themes (quote ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(diff-switches "-u")
 '(indent-tabs-mode nil)
 '(inhibit-default-init t)
 '(magit-diff-use-overlays nil)
 '(ns-command-modifier (quote control))
 '(recentf-max-saved-items 100)
 '(recentf-save-file "~/.emacs.d/recentf")
 '(vc-annotate-background nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(if (eq system-type 'darwin)
    (require 'cask "/usr/local/Cellar/cask/0.7.4/cask.el")
  (require 'cask "~/.cask/cask.el"))
  
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(when (file-exists-p "~/.emacs.d/arista.el") (load-file "~/.emacs.d/arista.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI                                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-splash-screen t)
(column-number-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(unless (and (eq system-type 'darwin) (display-graphic-p)) (menu-bar-mode -1))

(fset 'yes-or-no-p 'y-or-n-p)
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

(load-theme 'solarized t)

;; light in GUI, dark in terminal
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (let ((mode (if (display-graphic-p frame) 'light 'dark)))
              (set-frame-parameter frame 'background-mode mode)
              (set-terminal-parameter frame 'background-mode mode))
            (enable-theme 'solarized)))

;; make window divider prettier
(let ((display-table (or standard-display-table (make-display-table))))
  (set-display-table-slot display-table 'vertical-border (make-glyph-code ?│))
  (setq standard-display-table display-table))

(set-frame-parameter (selected-frame) 'alpha '(95 95))

(rich-minority-mode 1)
(setq rm-whitelist "Projectile")

;; Files
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/backups/\\1" t)))
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-auto-revert-mode 1)
(setq vc-follow-symlinks t)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")

(blink-cursor-mode 0)

;; Helm
(require 'helm)
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(define-key helm-map (kbd "C-k") 'helm-execute-persistent-action)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-x m") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;; (global-set-key (kbd "C-o") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-o") 'helm-semantic)
;; (global-set-key (kbd "C-o") 'helm-imenu)
(global-set-key (kbd "C-h a") 'helm-apropos)
;; (global-set-key (kbd "M-i") 'helm-swoop)
;; (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)

(setq helm-buffer-max-length nil)
(helm-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jcs-comment-box (b e)
  "Draw a box comment around the region but arrange for the region
to extend to at least the fill column. Place the point after the
comment box."
  (interactive "r")
  (save-restriction
    (narrow-to-region b e)
    (goto-char b)
    (end-of-line)
    (insert-char ?  (- fill-column (current-column)))
    (comment-box b (point-max) 1)
    (goto-char (point-max))))

;; NAWH
;; ; Delete words without adding to kill ring
;; (defun delete-word-backward (arg)
;;   (interactive "p")
;;   (delete-region (point) (progn (backward-word arg) (point))))
;; (defun delete-word-forward (arg)
;;   (interactive "p")
;;   (delete-region (point) (progn (forward-word arg) (point))))
;; (global-set-key (kbd "<M-backspace>") 'delete-word-backward)
;; (global-set-key (kbd "<C-backspace>") 'delete-word-backward)
;; (global-set-key (kbd "<M-d>") 'delete-word-backward)

(clean-aindent-mode t)
(define-key global-map (kbd "RET") 'newline-and-indent)

(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)
(global-undo-tree-mode 1)

;; flyspell needs location of aspell on Mac
(setq ispell-list-command "--list")
(when (eq system-type 'darwin)
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setq exec-path (append exec-path '("/usr/local/bin"))))

(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))
(global-set-key (kbd "M-[ q") 'comment-or-uncomment-line-or-region)
(global-set-key (kbd "C-;") 'comment-or-uncomment-line-or-region)

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(global-set-key (kbd "C-x C-x") 'exchange-point-and-mark-no-activate)

(setq require-final-newline t)

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
(global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)

; can keep C-u C-SPC C-SPC C-SPC
(setq set-mark-command-repeat-pop t)

(define-globalized-minor-mode global-highlight-symbol-mode
  highlight-symbol-mode (lambda () (progn (highlight-symbol-mode) (highlight-symbol-nav-mode))))
(setq highlight-symbol-idle-delay 0.5)
(global-highlight-symbol-mode)

(global-set-key (kbd "C-c s") 'toggle-truncate-lines)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.
  
  \(fn arg char)"
  'interactive)
(global-set-key (kbd "M-Z") 'zap-up-to-char)

(setq-default fill-column 80)

(setq ace-isearch-use-jump nil)
(global-ace-isearch-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(semantic-mode)
(global-semantic-stickyfunc-mode)

;; python
(add-hook 'python-mode-hook 'anaconda-mode)
;; (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
; company
;; (add-hook 'python-mode-hook 'company-mode)
(global-company-mode 1)
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))
(setq tab-always-indent 'complete)
(setq company-require-match nil)
(add-hook 'prog-mode-hook (lambda () (add-to-list 'completion-at-point-functions 'company-complete))) ; because python-mode overwrites it
(setq company-idle-delay nil)

;; Projectile
(setq projectile-completion-system 'helm)
(projectile-global-mode)
(helm-projectile-on)

;; apparently destroyed performance and caused core dumps
;; (add-hook 'prog-mode-hook 'which-function-mode)

(global-set-key (kbd "C-c l") 'nlinum-mode)

(add-hook 'prog-mode-hook (lambda () (progn
                                       (show-paren-mode 1)
                                       (setq show-paren-delay 0))))

(add-hook 'perl-mode-hook 'flycheck-mode)
(add-hook 'perl-mode-hook (lambda () (progn
                                       (define-key perl-mode-map (kbd "C-c C-d") 'cperl-perldoc)
                                       )))

(add-hook 'makefile-mode-hook (lambda () (setq indent-tabs-mode t)))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.vt\\'" . verilog-mode))

(eval-after-load 'asm-mode
  '(define-key asm-mode-map [(tab)] 'asm-indent-line))

;; guess offset, but don't need the global modeline
(dtrt-indent-mode 1)
(add-hook 'prog-mode-hook (lambda() (delete 'dtrt-indent-mode-line-info global-mode-string)))

(setq-default indent-tabs-mode nil)
(setq c-default-style "linux"
      c-basic-offset 3)
;; (add-to-list 'c-offsets-alist '(arglist-close . c-linup-close-paren))

(defun no-ns-indent ()
   (c-set-offset 'innamespace [0]))
(add-hook 'c++-mode-hook 'no-ns-indent)

(add-hook 'c-mode-common-hook
          (lambda() 
            (local-set-key (kbd "C-c o") 'ff-find-other-file)
            (local-set-key (kbd "C-c C-o") 'ff-find-other-file)
            ))

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc                                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Buffers
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "M-k") 'kill-this-buffer)

(global-set-key "\e0" 'delete-window)
(global-set-key "\e1" 'delete-other-windows)
(global-set-key "\e2" 'split-window)
(global-set-key "\e3" 'split-window-horizontally)
(global-set-key "\e5" 'query-replace-regexp)

;; Emacs server
(when (getenv "EMACS_SESSION_DIR") (setq server-socket-dir (getenv "EMACS_SESSION_DIR")))
(server-start)
