;; init-ui.el - UI / UX  -*- lexical-binding: t; -*-
;;
(use-package diminish)

(use-package doom-themes)
(elpaca-wait)
(use-package auto-dark	      ; theme selection following OS dark mode
  :diminish
  :config
  (setq auto-dark-dark-theme 'doom-acario-dark
	auto-dark-light-theme 'doom-acario-light
	auto-dark-allow-osascript t)
  (auto-dark-mode))

(tool-bar-mode -1)
(context-menu-mode)
(global-hl-line-mode)
(column-number-mode)

(setq mac-option-modifier 'meta)

(use-package zoom-window                ; temporarily zoom window
  :general ("C-x C-z" 'zoom-window-zoom))

(use-package helpful                    ; improved help windows
  :general
  ([remap describe-command]  'helpful-command)
  ([remap describe-function] 'helpful-callable)
  ([remap describe-key]      'helpful-key)
  ([remap describe-symbol]   'helpful-symbol)
  ([remap describe-variable] 'helpful-variable))

(use-package popper                     ; popups
  :general ("M-`"   'popper-toggle-latest)
  :custom (popper-reference-buffers
           '("\\*Messages\\*"
             "Output\\*$"
             "\\*Async Shell Command\\*"
             help-mode
             helpful-mode
             "\\*Apropos\\*"))
  :init (popper-mode) (popper-echo-mode)) ; For echo area hints


(provide 'init-ui)
