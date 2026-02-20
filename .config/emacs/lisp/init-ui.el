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

(use-package persistent-geometry :ensure nil :hook (kill-emacs . persistent-geometry-save))
(use-package minions :hook elpaca-after-init)
(use-package solaire-mode :hook (elpaca-after-init . solaire-global-mode))
(use-package ultra-scroll :hook elpaca-after-init)
(use-package zoom-window :general ("C-x C-z" 'zoom-window-zoom))
(use-package transient-posframe :hook elpaca-after-init)

(use-package which-key
  :hook elpaca-after-init
  :general ("C-h M-m" 'which-key-show-major-mode))
(use-package which-key-posframe
  :hook (which-key-mode . which-key-posframe-mode))

(use-package popper                     
  :hook (elpaca-after-init
         (popper-mode . popper-echo-mode))  
  :general ("M-`"   'popper-toggle
            "M-~"   'popper-toggle-type)
  :config
  (setq-union popper-reference-buffers
              '("\\*.*Shell Command.*\\*"
                help-mode
                )))


(provide 'init-ui)
