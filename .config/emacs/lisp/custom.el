;;; custom.el --- Customizations  -*- lexical-binding: t; -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a17a19a8af2561d170203c4e7d304fe21af4a221364f1012f932778a4f337922"
     "3a80596e4e60c19a549aad1fb5ab707c6b1e22650296bf87615f9f989274d9a4"
     "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     "7de64ff2bb2f94d7679a7e9019e23c3bf1a6a04ba54341c36e7cf2d2e56e2bcc"
     "166a2faa9dc5b5b3359f7a31a09127ebf7a7926562710367086fcc8fc72145da"
     "f1e8339b04aef8f145dd4782d03499d9d716fdc0361319411ac2efc603249326" default))
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
           (lambda nil
             (let ((message-log-max nil))
               (shell-command "pkill -USR1 -a kitty")))
           nil :local)
     (lisp-indent-local-overrides (cond . 0) (interactive . 0))
     (checkdoc-allow-quoting-nil-and-t . t)))
 '(warning-suppress-log-types
   '((copilot copilot-exceeds-max-char) (copilot copilot-no-mode-indent)
     (native-compiler) (emacs))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; Themes:
;; a17a19a8af2561d170203c4e7d304fe21af4a221364f1012f932778a4f337922  .config/emacs/elpaca/sources/batppuccin-emacs/batppuccin-macchiato-theme.el
;; 3a80596e4e60c19a549aad1fb5ab707c6b1e22650296bf87615f9f989274d9a4  .config/emacs/elpaca/sources/batppuccin-emacs/batppuccin-latte-theme.el
;; aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8  .config/emacs/elpaca/sources/themes/themes/doom-dark+-theme.el
;; 7de64ff2bb2f94d7679a7e9019e23c3bf1a6a04ba54341c36e7cf2d2e56e2bcc  .config/emacs/elpaca/sources/themes/themes/doom-acario-dark-theme.el
;; 166a2faa9dc5b5b3359f7a31a09127ebf7a7926562710367086fcc8fc72145da  .config/emacs/elpaca/sources/themes/themes/doom-acario-light-theme.el
;; f1e8339b04aef8f145dd4782d03499d9d716fdc0361319411ac2efc603249326  .config/emacs/elpaca/sources/themes/themes/doom-gruvbox-theme.el
