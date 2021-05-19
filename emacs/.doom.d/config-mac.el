(menu-bar-mode -1) ; needed on macos?
(when (fboundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode))
(mac-pseudo-daemon-mode)
(setq dired-use-ls-dired nil)
(map! "s-m" 'suspend-frame)
