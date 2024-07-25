;; config-fns.el: longer functions  -*- lexical-binding: t; -*-
;; ...copied in externally for workarounds, or internally and
;; tweaked. intentionally only definitions without anyhook-adding or
;; functionality-changing.

;; TODO: copy needed things over

(defun add-list-to-list (dst src) ; https://emacs.stackexchange.com/a/68048/26271
  "Similar to `add-to-list', but accepts a list as 2nd argument"
  (set dst
       (append (eval dst) src)))

(defmacro with-system (type &rest body) ; (with-system gnu/linux (...))
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

;; better C/M-w, from 2009 blog:
;; https://emacs-fu.blogspot.com/2009/11/copying-lines-without-selecting-them.html
(defadvice kill-ring-save (before slick-cut activate compile)
  "When called interactively with no active region, save a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defun kill-other-buffers ()       ; https://stackoverflow.com/a/3417473/3919508
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (cl-remove-if-not 'buffer-file-name (buffer-list))))
  (message "Killed all other buffers."))


(provide 'init-fns)
