;; -*- lexical-binding: t; -*-
;; early-init.el - Emacs early config

(setq package-enable-at-startup nil)        ; https://github.com/radian-software/straight.el#getting-started

(when (fboundp 'startup-redirect-eln-cache) ; https://github.com/emacscollective/no-littering#native-compilation-cache
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))
