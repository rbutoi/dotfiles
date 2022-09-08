;;; packages.el -*- lexical-binding: t; -*-
(unpin!
 counsel
 doom-modeline
 flycheck
 git-commit
 ivy
 magit
 notmuch
 swiper
 with-editor)

;; since:
;; $ doom up
;; [...]
;;   $ git clone --origin origin --no-checkout https\://git.notmuchmail.org/git/notmuch /home/radu/oss/doom-emacs/.local/straight/repos/notmuch/ --no-single-branch
;;   Cloning into '/home/radu/oss/doom-emacs/.local/straight/repos/notmuch'...
;;   fatal: unable to access 'https://git.notmuchmail.org/git/notmuch/': Failed to connect to git.notmuchmail.org port 443 after 86 ms: Connection refused
;;   [Return code: 128]
(package! notmuch :recipe (:host github :repo "notmuch/notmuch"))

(package! benchmark-init)
(package! buffer-move)
(package! default-text-scale)
(package! defrepeater)
(package! dired-hide-dotfiles)
(package! edit-server)
(package! eglot        :disable WORK)
(package! emacs-w3m)
(package! haskell-mode)
(package! highlight-thing)
(package! ivy-xref)
(package! journalctl-mode)
(package! keychain-environment)
(package! logview)
(package! lua-mode)
(package! meson-mode)
(package! notmuch-unread :recipe (:host github :repo "rbutoi/notmuch-unread"))
(package! outshine)
(package! password-store)
(package! pfuture)
(package! php-mode)
(package! pkgbuild-mode)
(package! project      :disable WORK)
(package! ripgrep)
(package! sql-indent)
(package! ssh-config-mode)
(package! string-inflection)
(package! systemd)
(package! tree-sitter)
(package! tree-sitter-langs)
(package! visual-fill-column)
(package! zoom-window)
(package! ztree)
