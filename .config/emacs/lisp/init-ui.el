;;; init-ui.el --- UI / UX  -*- lexical-binding: t; -*-

(general-add-hook 'elpaca-after-init-hook
                  '(context-menu-mode
                    global-hl-line-mode
                    column-number-mode
                    global-goto-address-mode
                    ;; repeat-mode
                    ))

(general-def
  "C-x C-m"   'execute-extended-command ; more convenient than M-x
  "C-x m"     'execute-extended-command
  "s-m"       'suspend-frame
  "C-x M-c"   (cmd! () (message "restarting...") (restart-emacs))
  "C-x C-M-c" 'save-buffers-kill-emacs
  "s-q"       'save-buffers-kill-emacs
  "C-s-f"     'toggle-frame-fullscreen)

;; Mode line
(use-package minions :hook elpaca-after-init)

(use-package moody :demand t
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-eldoc-minibuffer-message-function)
  (moody-replace-vc-mode))

(use-package solaire-mode
  :hook (elpaca-after-init . solaire-global-mode))

(use-package ultra-scroll :hook elpaca-after-init)

(use-package zoom-window :general ("C-x C-z" 'zoom-window-zoom))

(use-package which-key :hook elpaca-after-init)

(use-package popper                     
  :hook (elpaca-after-init
         (popper-mode . popper-echo-mode))  
  :general
  ("M-`"   'popper-toggle)
  ("M-~"   'popper-toggle-type)
  :config
  (dolist (m '(;; "Output\\*$"
               "\\*Async Shell Command\\*"
               help-mode
               ;; helpful-mode
               ;; "\\*Apropos\\*"
               ))
    (add-to-list 'popper-reference-buffers m)))


(provide 'init-ui)
