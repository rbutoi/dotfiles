;; debian is broken: https://discord.com/channels/406534637242810369/406554085794381833/817522711823646760
(remove-hook 'after-init-hook #'debian-ispell-set-startup-menu)

;; crostini is primarily used for email
(load (concat doom-private-dir "specific-notmuch.el"))
