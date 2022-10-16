;; -*- coding: utf-8; -*-
;;; Require:

;;; Code:
(defun jump-to-import()
  (interactive)
  ;; Rember position before jump.
  (cursor-position-1-store)
  ;; Jump to `import ...` position.
  (goto-char (point-min))
  (search-forward-regexp "\\(^import\\|^from\\)" nil t)
  )

(provide 'python-extension)

;;; python-extension.el ends here
