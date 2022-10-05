;;; -*- coding: utf-8-unix; lexical-binding: t; -*-

(let (
      ;; Make startup faster by reducing the frequency of garbage collection.
      ;; default (* 800 1000) bytes
      (gc-cons-threshold most-positive-fixnum)
      ;; default 0.1
      (gc-cons-percentage 0.6))

  ;; 加速启动
  (setq
   ;; 不缩放 frame
   frame-inhibit-implied-resize t
   ;; 默认用最简单的模式
   initial-major-mode 'fundamental-mode
   ;; 不要自动启用 package
   package-enable-at-startup nil
   package--init-file-ensured t)

  (when window-system
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (scroll-bar-mode -1))

  ;; map Win key and Hyper key to Super and Hyper for emacs on Windows.
  (when *win64*
    (setq w32-pass-lwindow-to-system nil)
    (setq w32-lwindow-modifier 'super) ; Left Windows key
    (setq w32-pass-rwindow-to-system nil)
    (setq w32-rwindow-modifier 'super) ; Right Windows key
    (w32-register-hot-key [s-])
    ;; (setq w32-pass-apps-to-system nil)
    (setq w32-apps-modifier 'hyper) ; Menu/App key
    (w32-register-hot-key [H-]))

  ;; font
  (when (and window-system *win64*)
    (let ((default-font (font-spec :name "Sarasa Mono SC"))
          (cn-font (font-spec :name "Sarasa Mono SC")))
      (set-face-attribute 'default nil :font default-font :height 116)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font t charset cn-font)))
    (set-face-font 'fixed-pitch "Sarasa Mono SC"))

  ;; from local extensions
  (require 'lsz-themes)
  (lsz-themes-load-style 'light)
  (require 'basic-toolkit)
  (require 'browse-kill-ring)
  (require 'lazy-load)

  (require 'init-auto-save)
  (require 'init-coding-system)
  (require 'init-dired)
  (require 'init-indent)
  (require 'init-generic)
  (require 'init-highlight-parentheses)
  (require 'init-idle)
  (require 'init-line-column-number)
  (if (not noninteractive) ;; disable lsp when export org site
    (require 'init-lsp-bridge))
  (require 'init-mode)
  (require 'init-org)
  (require 'init-proxy)
  (require 'init-time)
  (require 'init-undo-tree)

  ;; from submodules
  (require 'init-markdown-mode)
  (require 'init-rime)
  (require 'init-swiper)
  (require 'init-which-key)
  (require 'init-yasnippet)

  ;; Restore session at last.
  (require 'init-session)
  (emacs-session-restore)

  ;; load other plugins dynamically
  (require 'init-shortcut)

  ;; can be deferred loading
  ;; (run-with-idle-timer
  ;;  1 nil
  ;;  #'(lambda ()
  ;;      (require 'init-session)
  ;;      (emacs-session-restore)
  ;;      ))
)

(provide 'init)

;;; init.el ends here
