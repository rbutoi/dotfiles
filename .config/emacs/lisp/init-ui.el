;; init-ui.el - UI / UX  -*- lexical-binding: t; -*-

(setopt mac-option-modifier 'meta)

(tool-bar-mode -1)
(context-menu-mode)
(global-hl-line-mode)
(column-number-mode)

(global-goto-address-mode)

(add-hook 'elpaca-after-init-hook       ; game-changer
          (lambda () (desktop-save-mode) (desktop-read)))

;;;;;;;;;;;;;;;;;;;;;;
;; extenal packages ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package diminish)                  ; TODO: consider doom-modeline
(use-package doom-themes)
(elpaca-wait)

(use-package auto-dark	      ; theme selection following OS dark mode
  :diminish
  :config
  (setopt auto-dark-dark-theme 'doom-acario-dark
	  auto-dark-light-theme 'doom-acario-light
	  auto-dark-allow-osascript t)
  (auto-dark-mode))

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
  :custom
  (which-key-idle-secondary-delay 0.01)
  (which-key-show-docstrings t)
  :config
  ;; (which-key-setup-side-window-bottom)
  (which-key-mode))

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
