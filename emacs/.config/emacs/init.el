;; -*- lexical-binding: t; -*-
;; init.el - Emacs config

;;;; Prologue

;; package management: straight.el / use-package
(setq straight-use-package-by-default t)
;; broken on venv error?
;; straight-check-for-modifications '(watch-files find-when-checking) ; perf

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; benchmark
(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))
(add-hook 'after-init-hook
          (lambda () (message "loaded in %s" (emacs-init-time))))

;; install some needed packages
(straight-use-package 'general)

;;;; UI

;; set theme/font using customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-vivendi)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 100 :family "JetBrains Mono")))))

;;;; Buffers and windows
(general-define-key
 "M-l"       (lambda () (interactive) (select-window (get-mru-window t t t)))
 "M-0"       'delete-window
 :keymaps '(global magit-mode-map)      ; just drop the M- in magit
 "M-1"       'delete-other-windows
 "M-2"       'split-window-below
 "M-3"       'split-window-right
 "M-o"       (lambda () (interactive) (other-window +1))
 "M-i"       (lambda () (interactive) (other-window -1))
 ;; "C-S-k"     'doom/kill-other-buffers
 ;; "C-x M-k"   'doom/kill-other-buffers
 ;; "C-S-M-k"   'doom/kill-all-buffers
 ;; "C-x C-M-k" 'doom/kill-all-buffers
 )

(tool-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; modeline
(use-package doom-modeline
  :init
  (setq doom-modeline-support-imenu t)
  (doom-modeline-mode 1))
(use-package all-the-icons)

;;;; Editing
(use-package ws-butler                  ; delete trailing whitespace
  :config
  (setq ws-butler-keep-whitespace-before-point nil)
  (ws-butler-global-mode))

;;;; Programming

(setq-default indent-tabs-mode nil)
(setq vc-follow-symlinks t)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(defun my/should-be-prog-mode () (run-mode-hooks 'prog-mode-hook))
(add-hook 'emacs-lisp-mode-hook 'my/should-be-prog-mode)

(use-package magit
  :config
  (setq magit-repository-directories
        `(("~/dotfiles" . 0)
          ("~/dotfiles-google" . 0)
          ("~/oss" . 1)
          ("~/.emacs.d" . 0))
        magit-log-auto-more t
        magit-log-margin '(t "%a %b %d %Y" magit-log-margin-width t 18))
  ;; (map! "C-x   g"   'magit-status
  ;;       "C-x C-g"   'magit-status
  ;;       "C-x C-M-g" 'magit-list-repositories)
  )

;; uncat

(setq confirm-kill-processes nil)

;; emacs utils:
(use-package restart-emacs)
  ;; :bind

;;;; Epilogue
;;(load (concat doom-private-dir "config-fns.el"))     ; useful function defininitons

