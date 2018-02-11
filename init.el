; Here in init.el so changes to settings.org don't always ask this question

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq vc-follow-symlinks t)
(load "~/.emacs.d/custom.el")
      ;; 'no-error 'no-message

(org-babel-load-file "~/.emacs.d/settings.org")
