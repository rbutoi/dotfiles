;; init-ui.el - UI / UX  -*- lexical-binding: t; -*-

;; Font, theme
(set-face-attribute 'default nil :family "Iosevka Fixed" :height 130)
(use-package ef-themes
  :hook
  (elpaca-after-init . (lambda () (load-theme 'ef-owl t))))

(context-menu-mode)
(global-hl-line-mode)
(column-number-mode)

(global-goto-address-mode)

(add-hook 'elpaca-after-init-hook       ; restore buffers and frames on startup
          (lambda () (desktop-save-mode) (desktop-read)) -10)

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

(use-package diminish)                  ; TODO: consider doom-modeline

(use-package zoom-window                ; temporarily zoom window
  :general ("C-x C-z" 'zoom-window-zoom))

(use-package helpful                    ; improved help windows
  :general
  ([remap describe-command]  'helpful-command)
  ([remap describe-function] 'helpful-callable)
  ([remap describe-key]      'helpful-key)
  ([remap describe-symbol]   'helpful-symbol)
  ([remap describe-variable] 'helpful-variable))

(use-package which-key                  ; useful shortcut reminders
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
