;; -*- coding: utf-8; -*-
;;; Require:
(require 'auto-save)

;;; Code:
;; auto save after <number> second(s)
;; (setq auto-save-idle 1)
(setq auto-save-silent t)
(auto-save-enable)

(provide 'init-auto-save)

;;; init-auto-save.el ends here