;; -*- coding: utf-8; -*-
;;; Require:

;;; Code:
;; show column number on Mode line
(column-number-mode t)

;; Line numbers are not displayed when large files are used.
(setq line-number-display-limit large-file-warning-threshold)
(setq line-number-display-limit-width 1000)

(dolist (hook (list 'c-mode-common-hook
                    'c-mode-hook
                    'llvm-mode-hook
                    'cmake-mode-hook
                    'qmake-mode-hook
                    'makefile-gmake-mode-hook

                    'rust-mode-hook

                    'org-mode-hook
                    'markdown-mode-hook

                    'emacs-lisp-mode-hook
                    'lisp-interaction-mode-hook
                    'lisp-mode-hook
                    'slime-repl-mode-hook
                    'elixir-mode-hook
                    'clojure-mode-hook

                    'haskell-mode-hook

                    'java-mode-hook
                    'go-mode-hook

                    'sh-mode-hook
                    'lua-mode-hook
                    'python-mode-hook
                    'js-mode-hook
                    'typescript-mode-hook
                    'coffee-mode-hook
                    'html-mode-hook
                    'css-mode-hook
                    'php-mode-hook
                    'web-mode-hook

                    'nxml-mode-hook
                    'conf-toml-mode-hook
                    'conf-windows-mode-hook
                    'yaml-mode-hook
                    'nginx-mode-hook
                    'conf-mode-hook

                    'package-menu-mode-hook

                    'asm-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode))))

(provide 'init-line-column-number)

;;; init-line-column-number.el ends here
