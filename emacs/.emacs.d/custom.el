(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   ("#3b6b40f432d6" "#07b9463c4d36" "#47a3341e358a" "#1d873c3f56d5" "#2d86441c3361" "#43b7362d3199" "#061d417f59d7"))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#5b7300" . 20)
     ("#007d76" . 30)
     ("#0061a8" . 50)
     ("#866300" . 60)
     ("#992700" . 70)
     ("#a00559" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#866300" "#992700" "#a7020a" "#a00559" "#243e9b" "#0061a8" "#007d76" "#5b7300")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(lsp-ui-doc-border "#93a1a1")
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#5b7300" "#b3c34d" "#0061a8" "#2aa198" "#d33682" "#6c71c4")))
 '(org-journal-dir "~/google-drive/journal/")
 '(package-selected-packages
   (quote
    (goto-chg helm-find helm auto-package-update markdown-mode json-mode yaml-mode racer expand-region vc-hgcmd helm-rg diff-hl company s ztree zoutline zoom-window yafolding which-key use-package undo-tree swiper string-inflection solarized-theme smartparens sane-term rust-mode rich-minority posframe pos-tip pkgbuild-mode org-journal nlinum mwim magit keychain-environment iedit hydra helm-swoop helm-projectile helm-descbinds golden-ratio fill-column-indicator elisp-format dtrt-indent clang-format buffer-move ace-window)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(recentf-auto-cleanup (quote never))
 '(safe-local-variable-values
   (quote
    ((eval switch-org-and-elisp)
     (eval progn
           (setq switch-org-and-elisp-map
                 (make-sparse-keymap))
           (define-minor-mode switch-org-and-elisp-mode "" nil nil switch-org-and-elisp-map)
           (bind-key "C-c m"
                     (lambda nil
                       (interactive)
                       (if
                           (string=
                            (quote emacs-lisp-mode)
                            major-mode)
                           (progn
                             (org-mode)
                             (switch-org-and-elisp-mode t))
                         (progn
                           (emacs-lisp-mode)
                           (switch-org-and-elisp-mode t))))
                     switch-org-and-elisp-map)
           (emacs-lisp-mode)
           (switch-org-and-elisp-mode 1))
     (eval progn
           (setq switch-org-and-elisp-map
                 (make-sparse-keymap))
           (define-minor-mode switch-org-and-elisp-mode "" nil nil switch-org-and-elisp-map)
           (bind-key "C-c m"
                     (lambda nil
                       (interactive)
                       (if
                           (string=
                            (quote emacs-lisp-mode)
                            major-mode)
                           (progn
                             (org-mode)
                             (switch-org-and-elisp-mode t))
                         (progn
                           (emacs-lisp-mode)
                           (switch-org-and-elisp-mode t))))
                     switch-org-and-elisp-map)
           (switch-org-and-elisp-mode 1)))))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4366eb20b4")
     (60 . "#c1167942154f")
     (80 . "#b58900")
     (100 . "#a6ae8f7c0000")
     (120 . "#9ed892380000")
     (140 . "#96be94cf0000")
     (160 . "#8e5397440000")
     (180 . "#859900")
     (200 . "#77679bfc4635")
     (220 . "#6d449d465bfd")
     (240 . "#5fc09ea47092")
     (260 . "#4c68a01784aa")
     (280 . "#2aa198")
     (300 . "#303498e7affc")
     (320 . "#2fa1947cbb9b")
     (340 . "#2c879008c736")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#a7020a" "#dc322f" "#5b7300" "#859900" "#866300" "#b58900" "#0061a8" "#268bd2" "#a00559" "#d33682" "#007d76" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
