;; -*- lexical-binding: t; -*-
;; custom.el - Emacs Easy Customize config

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   '("9e1cf0f16477d0da814691c1b9add22d7cb34e0bb3334db7822424a449d20078" "3e374bb5eb46eb59dbd92578cae54b16de138bc2e8a31a2451bf6fdb0f3fd81b" "467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2"))
 '(safe-local-variable-values
   '((eval progn
           (column-enforce-mode))
     (eval progn
           (outshine-mode)
           (column-enforce-mode))
     (eval font-lock-add-keywords nil
           `((,(concat "("
                       (regexp-opt
                        '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
                        t)
                       "\\_>")
              1 'font-lock-variable-name-face)))
     (org-src-preserve-indentation)
     (eval require 'ol-man nil t)
     (eval require 'magit-base nil t)
     (toc-org-max-depth . 4)))
 '(warning-suppress-log-types '((emacs))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 130 :family "Iosevka")))))
