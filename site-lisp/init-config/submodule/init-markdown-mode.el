;; -*- coding: utf-8; -*-
;;; Require:
(require 'markdown-mode)
;; (require 'olivetti)

;;; Code:
(dolist (hook (list
               'markdown-mode-hook
               ))
;;   (add-hook hook
;;             #'(lambda ()
;;                 (olivetti-mode 1)
;;                 (olivetti-set-width 120)
;;                 ))
                )

(provide 'init-markdown-mode)

;;; init-markdown-mode.el ends here
