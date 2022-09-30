;;; Require
(require 'go-mode)

;;; Code:
(defun go-run-buffer()
  (interactive)
  (shell-command (concat "go run " (buffer-name))))

(lazy-load-unset-keys
 '("C-k" "M-o")
 go-mode-map)

(lazy-load-set-keys
 '(
   ("C-c C-c" . go-run-buffer)
   ("C-c C-f" . gofmt)
   ("C-c C-d" . godoc)
   ("C-c C-a" . go-import-add)
   )
 go-mode-map)

(provide 'init-go-mode)

;;; init-go-mode.el ends here
