;; -*- coding: utf-8; -*-
;;; Require:

;;; Code:
;; show column number on Mode line
(column-number-mode t)

;; Line numbers are not displayed when large files are used.
(setq line-number-display-limit large-file-warning-threshold)
(setq line-number-display-limit-width 1000)

(dolist (hook (list 'org-mode-hook
                    'c-mode-common-hook
                    'c-mode-hook
                    'emacs-lisp-mode-hook
                    'lisp-interaction-mode-hook
                    'lisp-mode-hook
                    'java-mode-hook
                    'asm-mode-hook
                    'haskell-mode-hook
                    'rcirc-mode-hook
                    'erc-mode-hook
                    'sh-mode-hook
                    'makefile-gmake-mode-hook
                    'python-mode-hook
                    'js-mode-hook
                    'html-mode-hook
                    'css-mode-hook
                    'tuareg-mode-hook
                    'go-mode-hook
                    'coffee-mode-hook
                    'qml-mode-hook
                    'markdown-mode-hook
                    'slime-repl-mode-hook
                    'package-menu-mode-hook
                    'cmake-mode-hook
                    'php-mode-hook
                    'web-mode-hook
                    'coffee-mode-hook
                    'sws-mode-hook
                    'jade-mode-hook
                    'vala-mode-hook
                    'rust-mode-hook
                    'ruby-mode-hook
                    'qmake-mode-hook
                    'lua-mode-hook
                    'swift-mode-hook
                    'llvm-mode-hook
                    'conf-toml-mode-hook
                    'nxml-mode-hook
                    'nim-mode-hook
                    'typescript-mode-hook
                    'elixir-mode-hook
                    'clojure-mode-hook
                    'yaml-mode
                    ))
  (add-hook hook (lambda () (display-line-numbers-mode))))

(provide 'init-line-column-number)

;;; init-line-column-number.el ends here
