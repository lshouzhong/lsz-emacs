;; -*- coding: utf-8; -*-
;;; Require:
(require 'lsp-bridge)
(require 'lsp-bridge-jdtls)

;;; Code:
(global-lsp-bridge-mode)

;; disable wen ( chinese ) lsp-server for org-mode
(assq-delete-all 'org-mode lsp-bridge-single-lang-server-mode-list)

;; ;; disable yasnippet completion
(setq acm-enable-yas nil)

;; (setq acm-enable-tabnine-helper nil)

;; 融合 `lsp-bridge' `find-function' 以及 `dumb-jump' 的智能跳转
(defun lsp-bridge-jump ()
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (let ((symb (function-called-at-point)))
      (when symb
        (find-function symb))))
   (lsp-bridge-mode
    (lsp-bridge-find-def))
   (t
    (require 'dumb-jump)
    (dumb-jump-go))))

(defun lsp-bridge-jump-back ()
  (interactive)
  (cond
   (lsp-bridge-mode
    (lsp-bridge-return-from-def))
   (t
    (require 'dumb-jump)
    (dumb-jump-back))))

;; show logs ( only developing needed )
;; (setq lsp-bridge-enable-log t)

(provide 'init-lsp-bridge)

;;; init-lsp-bridge.el ends here
