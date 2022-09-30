;; -*- coding: utf-8; -*-
;;; Require:
;; (require 'force-indent)

;;; Code:
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(defun adjust-languages-indent (n)
  (setq force-indent-size n)

  (setq-local c-basic-offset n)

  (setq-local coffee-tab-width n)
  (setq-local javascript-indent-level n)
  (setq-local js-indent-level n)
  (setq-local js2-basic-offset n)

  (setq-local web-mode-attr-indent-offset n)
  (setq-local web-mode-attr-value-indent-offset n)
  (setq-local web-mode-code-indent-offset n)
  (setq-local web-mode-css-indent-offset n)
  (setq-local web-mode-markup-indent-offset n)
  (setq-local web-mode-sql-indent-offset n)

  (setq-local css-indent-offset n))

(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'haskell-mode-hook
               'asm-mode-hook
               'sh-mode-hook
               'haskell-cabal-mode-hook
               'ruby-mode-hook
               'qml-mode-hook
               'scss-mode-hook
               'coffee-mode-hook
               ))
  (add-hook hook #'(lambda ()
                     (setq indent-tabs-mode nil)
                     (adjust-languages-indent 4)
                     )))

(dolist (hook (list
               'web-mode-hook
               'js-mode-hook
               ))
  (add-hook hook #'(lambda ()
                     (setq indent-tabs-mode nil)
                     (adjust-languages-indent 2)
                     )))

(provide 'init-force-indent)

;;; init-force-indent.el ends here
