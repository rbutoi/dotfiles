;; config-fns.el: longer functions  -*- lexical-binding: t; -*-
;; ...copied in externally for workarounds, or internally and
;; tweaked. intentionally only definitions without anyhook-adding or
;; functionality-changing.

;; TODO: copy needed things over

(defun kill-other-buffers ()	; https://stackoverflow.com/a/3417473/3919508
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (cl-remove-if-not 'buffer-file-name (buffer-list)))))

(provide 'init-fns)
