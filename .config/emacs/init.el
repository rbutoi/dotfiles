;; init.el - Emacs config  -*- lexical-binding: t; -*-

;; Set load path and initialize basic Emacs settings
(load (expand-file-name "lisp/init-emacs.el" user-emacs-directory))
(require 'init-fns)                     ; some convenience functions

(require 'init-ui)
(require 'init-buffers)
(require 'init-editing)
(require 'init-programming)
(require 'init-langs)
(require 'init-ext)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7b8f5bbdc7c316ee62f271acf6bcd0e0b8a272fdffe908f8c920b0ba34871d98" "b9c002dc827fb75b825da3311935c9f505d48d7ee48f470f0aa7ac5d2a595ab2" "9d8d60e69db09062e759f008e59fac1d76618b09cddaf06cef093653c16f1e75" default))
 '(safe-local-variable-values
   '((eval when
           (fboundp 'rainbow-mode)
           (rainbow-mode 1))
     (elisp-lint-indent-specs
      (git-gutter:awhen . 1)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
