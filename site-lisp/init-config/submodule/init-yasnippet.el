;; -*- coding: utf-8; -*-
;;; Require:
(require 'yasnippet)

;;; Code:
(setq yas-snippet-dirs (list (concat lsz-emacs-root-dir "/snippets")))
(yas-global-mode 1)

;; Disable yasnippet mode on some mode.
(dolist (hooks (list
                'term-mode-hook
                ))
  (add-hook hooks #'(lambda () (yas-minor-mode -1))))

(provide 'init-yasnippet)

;;; init-yasnippet.el ends here
