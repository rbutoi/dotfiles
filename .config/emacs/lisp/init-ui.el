;;; init-ui.el --- UI / UX  -*- lexical-binding: t; -*-

(use-package general :demand t :ensure (:wait t)) ; keybinds

(context-menu-mode)
(global-hl-line-mode)
(column-number-mode)
(global-goto-address-mode)
(general-add-hook '(prog-mode-hook text-mode-hook)
                  'display-line-numbers-mode)

(setopt inhibit-startup-screen  t
        initial-scratch-message "")

(general-def
  "C-x C-m"   'execute-extended-command ; more convenient than M-x
  "C-x m"     'consult-mode-command
  "s-m"       'suspend-frame
  "C-x M-c"   (cmd! () (message "restarting...") (restart-emacs))
  "C-x C-M-c" 'save-buffers-kill-emacs
  "C-s-f"     'toggle-frame-fullscreen)

(use-package persistent-geometry :ensure ; save window geometry across restarts
  nil :hook (kill-emacs . persistent-geometry-save))
(use-package minions                    ; hiding list of minor modes in modeline
  :hook elpaca-after-init)
(use-package solaire-mode               ; differentiate background of non-file buffers
  :hook (elpaca-after-init . solaire-global-mode))
(use-package ultra-scroll               ; pixel-smooth scrolling
  :hook elpaca-after-init) 
(use-package zoom-window                ; temporarily show one window
  :general ("C-x C-z" 'zoom-window-zoom)) 
(use-package transient-posframe         ; popups in middle of emacs
  :hook elpaca-after-init) 

(use-package which-key                  ; show available keybinds after a prefix
  :hook elpaca-after-init
  :general ("C-h M-m" 'which-key-show-major-mode))
(use-package which-key-posframe         ; ... as a popup
  :hook (which-key-mode . which-key-posframe-mode))

(use-package helpful                    ; improved help windows
  :general
  ([remap describe-command]  'helpful-command)
  ([remap describe-function] 'helpful-callable)
  ([remap describe-key]      'helpful-key)
  ([remap describe-symbol]   'helpful-symbol)
  ([remap describe-variable] 'helpful-variable))

(use-package popper                     ; make ephemeral windows [...]
  :hook (elpaca-after-init
         (popper-mode . popper-echo-mode))  
  :general ("M-`"   'popper-toggle
            "M-~"   'popper-toggle-type)
  :config
  (setq-union popper-reference-buffers
              '("\\*.*Shell Command.*\\*"
                help-mode
                helpful-mode)))


(provide 'init-ui)
