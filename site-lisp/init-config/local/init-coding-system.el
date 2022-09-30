;; -*- coding: utf-8; -*-
;;; Require:

;;; Code:
(set-language-environment 'UTF-8)

(set-default-coding-systems 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(set-file-name-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-next-selection-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)

(when *win64*
  (set-next-selection-coding-system 'utf-16-le)
  (set-selection-coding-system 'utf-16-le)
  (set-clipboard-coding-system 'utf-16-le))

(when (or *linux* *unix* *is-a-mac*)
  (set-clipboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8))

;; 排序在最后的最先被选择
(prefer-coding-system 'cp950)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)
(prefer-coding-system 'gb18030)
(prefer-coding-system 'utf-16)
(prefer-coding-system 'utf-8-dos)
(prefer-coding-system 'utf-8-unix)

(provide 'init-coding-system)

;;; init-coding-system ends here