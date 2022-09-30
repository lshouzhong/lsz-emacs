;; -*- coding: utf-8; -*-
;;; Require:
(require 'python)

;;; Code:
(lazy-load-local-keys
 '(
   ("C-S-j" . jump-to-import)
   )
 python-mode-map
 "python-extension")

(provide 'init-python)

;;; init-python.el ends here
