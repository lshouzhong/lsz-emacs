;; -*- coding: utf-8; -*-
;;; Require:

;;; Code:
;; Restore emacs session.
(setq initial-buffer-choice t)
(run-with-timer 1 nil #'(lambda () (bury-buffer)))

;; turn on word-wrap in all buffers
;; (global-visual-line-mode)
(setq word-wrap t)

(setq system-time-locale "C") ;; use Posix format for time string

(setq inhibit-startup-message t) ;; do not show welcome page

(show-paren-mode t) ;; show bracket pairing
(global-auto-revert-mode t) ;; auto load the changed file
(electric-pair-mode t) ;; automatic completion of parentheses
(delete-selection-mode t) ;; delete selected text when input on it

;; Improve long line processing performance since emacs 27
(setq bidi-inhibit-bpa t)
(setq-default bidi-paragraph-direction 'left-to-right)

;; increase IO performance
(setq process-adaptive-read-buffering nil)
(setq read-process-output-max (* 1024 1024))

;; replace yes/no with y/n
;; but since emacs 28 user can use (use-short-answers t) to do it
(fset 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode t)                 ;; highlight current line
(blink-cursor-mode -1)                  ;; cursor not blink
(global-subword-mode 1)                 ;Word移动支持 FooBar 的格式
(setq use-dialog-box nil)               ;; never pop dialog
(setq inhibit-startup-screen t)         ;; inhibit start screen
(setq initial-scratch-message "") ;关闭启动空白buffer, 这个buffer会干扰session恢复
(setq-default comment-style 'indent)    ;设定自动缩进的注释风格
(setq ring-bell-function 'ignore)       ;; disable the ring bell ( the sound is annoying especially when an error occurs )
(setq default-major-mode 'text-mode)    ;; set `text-mode' as default major-mode
(setq mouse-yank-at-point t)            ;; click mouse-2 to past the content of clipboard to cursor position, not mouse pointer position
(setq x-select-enable-clipboard t)      ;支持emacs和外部程序的粘贴
(setq split-width-threshold nil)        ;分屏的时候使用上下分屏
(setq inhibit-compacting-font-caches t) ;; use font caches to avoid UI no response
(setq confirm-kill-processes nil)       ;退出自动杀掉进程
(setq word-wrap-by-category t)          ;; optimize CJK characters wraping since emacs 28
(add-hook 'find-file-hook 'highlight-parentheses-mode t) ;增强的括号高亮

(setq ad-redefinition-action 'accept)   ;不要烦人的 redefine warning
(setq frame-resize-pixelwise t) ;设置缩放的模式,避免Mac平台最大化窗口以后右边和下边有空隙

;; 平滑地进行半屏滚动，避免滚动后recenter操作
(setq scroll-step 1
      scroll-conservatively 10000)

;; 不显示 *scratch*
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; ;; Don't ask me when close emacs with process is running
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-flet ((process-list ())) ad-do-it))

;; Don't ask me when kill process buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(provide 'init-generic)

;;; init-generic.el ends here
