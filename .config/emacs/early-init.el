;; package manager: Elpaca (https://github.com/progfolio/elpaca)
(setq package-enable-at-startup nil)
(load (expand-file-name "lisp/init-elpaca.el" user-emacs-directory))
(elpaca elpaca-use-package
        (elpaca-use-package-mode)
        (setq use-package-always-ensure t))

;; UI speedups
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
