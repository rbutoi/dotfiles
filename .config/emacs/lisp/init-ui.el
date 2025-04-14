;; init-ui.el - UI / UX  -*- lexical-binding: t; -*-

(set-face-attribute 'default nil :family "Iosevka" :height 130)

(context-menu-mode)
(global-hl-line-mode)
(column-number-mode)

(global-goto-address-mode)

;;;;;;;;;;;;;;
;; keybinds ;;
;;;;;;;;;;;;;;

(setopt mac-option-modifier  'meta
        mac-command-modifier 'super)

(general-def
  "C-x C-m"   'execute-extended-command ; more convenient than M-x
  "C-x m"     'execute-extended-command
  "s-m"       'suspend-frame
  "C-x M-c"   'restart-emacs
  "C-x C-M-c" 'save-buffers-kill-emacs
  "s-q"       'save-buffers-kill-emacs
  "C-s-f"     'toggle-frame-fullscreen)

;;;;;;;;;;;;;;;;;;;;;;
;; extenal packages ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package ef-themes)
(use-package auto-dark
  :diminish
  :init (auto-dark-mode)
  :custom
  (auto-dark-themes '((ef-owl) (ef-eagle)))
  (auto-dark-allow-osascript t))

(use-package easysession
  :diminish
  :commands (easysession-switch-to
             easysession-save-as
             easysession-save-mode
             easysession-load-including-geometry)
  :custom
  (easysession-save-interval (* 60 60)) ; Save every hour
  :init
  (add-hook 'elpaca-after-init-hook #'easysession-load-including-geometry 102)
  (add-hook 'elpaca-after-init-hook #'easysession-save-mode 103)
  :config
  (diminish 'easysession-save-mode))

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
