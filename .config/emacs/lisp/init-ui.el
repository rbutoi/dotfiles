;; init-ui.el - UI / UX  -*- lexical-binding: t; -*-

(set-face-attribute 'default nil :family "Iosevka" :height 140)

(context-menu-mode)
(global-hl-line-mode)
(column-number-mode)

(global-goto-address-mode)

;;;;;;;;;;;;;;
;; keybinds ;;
;;;;;;;;;;;;;;

(repeat-mode)

(general-def
  "C-x C-m"   'execute-extended-command ; more convenient than M-x
  "C-x m"     'execute-extended-command
  "s-m"       'suspend-frame
  "C-x M-c"   (cmd! () (message "restarting...") (restart-emacs))
  "C-x C-M-c" 'save-buffers-kill-emacs
  "s-q"       'save-buffers-kill-emacs
  "C-s-f"     'toggle-frame-fullscreen)

;;;;;;;;;;;;;;;;;;;;;;
;; extenal packages ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package doom-themes
  :config
  (load-theme 'doom-acario-light))

(use-package solaire-mode
  :config
  (solaire-global-mode))

(use-package ultra-scroll
  :config
  (ultra-scroll-mode 1))

(use-package diminish)                  ; TODO: consider doom-modeline

(use-package zoom-window                ; temporarily zoom window
  :general ("C-x C-z" 'zoom-window-zoom))

(use-package which-key                  ; useful shortcut reminders
  :defer 2
  :diminish
  :config (which-key-mode))

(use-package popper                     ; popups
  :general
  ("M-`"   'popper-toggle)
  ("M-~"   'popper-toggle-type)
  :custom (popper-reference-buffers
           '("\\*Messages\\*"
             "Output\\*$"
             "\\*Async Shell Command\\*"
             help-mode
             helpful-mode
             "\\*Apropos\\*"))
  :init
  (popper-mode)
  (popper-echo-mode))


(provide 'init-ui)
