;; -*- lexical-binding: t; -*-

;; retina display and all that: 14 -> 13
(setq doom-font (font-spec :family "JetBrains Mono" :size 13))

;; only in Mitsuharu fork: render ligarures (-> != >= et al)
(when (fboundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode))

(map! "s-m" 'suspend-frame)  ; mirror MacOS ⌘-m for minimize
