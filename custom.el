(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(ansi-color-names-vector
   ;; ["#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"])
 ;; '(ansi-term-color-vector
   ;; [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"] t)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "c72a772c104710300103307264c00a04210c00f6cc419a79b8af7890478f380e" "43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" "31992d4488dba5b28ddb0c16914bf5726dc41588c2b1c1a2fd16516ea92c1d8e" "44c566df0e1dfddc60621711155b1be4665dd3520b290cb354f8270ca57f8788" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" default)))
 '(doc-view-continuous t)
 '(doc-view-resolution 300)
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-agenda-files (quote ("~/Documents/google-drive/journal/20170308")))
 '(package-selected-packages
   (quote
    (clang-format protobuf-mode esup yasnippet-snippets yasnippet twilight-bright-theme helm-projectile projectile helm-rtags rtags company-irony-c-headers sane-term cmake-mode flycheck-irony company-irony irony exec-path-from-shell solarized-theme helm-swoop smartparens flatui-theme leuven-theme eink-theme github-modern-theme zenburn-theme markdown-preview-mode markdown-mode auctex ztree pdf-tools itail magit buffer-move zoom-window yafolding which-key visual-fill-column undo-tree term-run rust-mode rich-minority pkgbuild-mode pallet org-journal nlinum mwim keychain-environment helm-unicode helm-descbinds helm-ag haskell-mode golden-ratio flycheck-rust dtrt-indent dired+ diffview company-auctex company-anaconda clean-aindent-mode ag ack ace-window)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(safe-local-variable-values
   (quote
    ((eval progn
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
 ;; '(term-default-bg-color "#002b36")
 ;; '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
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
