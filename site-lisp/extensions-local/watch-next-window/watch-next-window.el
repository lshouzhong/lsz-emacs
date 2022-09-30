;; -*- coding: utf-8; -*-
;;; Require:

;;; Code:
;; "up" to see previous content
;; "down" to see further content
(defun watch-next-window-up ()
  (interactive)
  (watch-next-window-internal "up"))

(defun watch-next-window-down ()
  (interactive)
  (watch-next-window-internal "down"))

(defun watch-next-window-up-line ()
  (interactive)
  (watch-next-window-internal "up" 1))

(defun watch-next-window-down-line ()
  (interactive)
  (watch-next-window-internal "down" 1))

(defun watch-next-window-internal (direction &optional line)
  (save-excursion
    ;; Switch to next window.
    (other-window 1)
    ;; Do scroll operation.
    (ignore-errors
      (if (string-equal direction "up")
          (if line
              (scroll-up line)
            (scroll-up))
        (if line
            (scroll-down line)
          (scroll-down))))
    ;; Switch back to current window.
    (other-window -1)
    ))

(provide 'watch-next-window)

;;; watch-next-window.el ends here
