;; init-ui.el - UI / UX  -*- lexical-binding: t; -*-

(set-face-attribute 'default nil :family "Iosevka" :height 130)

(tool-bar-mode -1)
(context-menu-mode)
(global-hl-line-mode)
(column-number-mode)

(global-goto-address-mode)

(add-hook 'elpaca-after-init-hook
          (lambda ()
            (desktop-save-mode)         ; game-changer
            (desktop-read)))

(setopt mac-option-modifier 'meta)

;;;;;;;;;;;;;;;;;;;;;;
;; extenal packages ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package ef-themes :config (load-theme 'ef-owl))

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
