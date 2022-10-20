;; -*- coding: utf-8; -*-

;;; There are 5 top sections:
;;; - unset keys
;;; - extensions
;;; - extensions-local
;;; - shortcut on functions in init-*.el
;;; - shortcut on built-in function

;;; level 1 section start with 12 dash
;;; level 2 section start with 6 dash
;;; level 3 section start with 3 dash
;;; level 4 section start with 1 dash

;;; ------------ unset keys
(lazy-load-unset-keys
 '("C-z" "C-q" "s-W" "C-\\" "s-c"
   "s-z" "s-x" "s-v" "M-h" "C-6"))



;;; ------------ extensions

;; ------ ace-window
(lazy-load-global-keys
 '(
   ("M-o" . ace-window)
   )
 "init-ace-window")

;; ------ avy
(lazy-load-global-keys
 '(
   ("M-g c" . avy-goto-char)
   ("M-g w" . avy-goto-word-1)
   ("M-g l" . avy-goto-line)
   )
 "avy")

;; ------ emacs-neotree
(lazy-load-global-keys
 '(
   ("s-n" . neotree-toggle)
   )
 "init-neotree")

;; ------ iedit
(lazy-load-global-keys
 '(
   ("s-;" . iedit-mode)
   )
 "init-iedit")

;; ------ lsp-bridge
(lazy-load-global-keys
 '(
   ("C-6" . lsp-bridge-lookup-documentation)
   ("C-7" . lsp-bridge-jump-back)
   ("C-8" . lsp-bridge-jump)
   ("C-9" . lsp-bridge-find-references)
   ("M-s-j" . lsp-bridge-jump-to-next-diagnostic)
   ("M-s-k" . lsp-bridge-jump-to-prev-diagnostic)
   ("M-s-l" . lsp-bridge-ignore-current-diagnostic)
   ("M-s-n" . lsp-bridge-popup-documentation-scroll-up)
   ("M-s-p" . lsp-bridge-popup-documentation-scroll-down)
   )
 "init-lsp-bridge")

;; ------ multiple cursors
(lazy-load-global-keys
 '(
   ("C-M-<" . mc/mark-previous-like-this)
   ("C-M->" . mc/unmark-previous-like-this)
   ("s-<" . mc/mark-next-like-this)
   ("s->" . mc/unmark-next-like-this)
   ("C-s-," . mc/edit-beginnings-of-lines)
   ("s-S-<mouse-1>" . mc/add-cursor-on-click)
   )
 "multiple-cursors")

;; ------ rime
(lazy-load-global-keys
 '(
   ("C-c C-\\" . toggle-input-method)
   )
 "init-rime")



;;; ------------ extensions-local

;; ------ basic-toolkit
;; --- basic-toolkit
;; - 基础工具库
(lazy-load-global-keys
 '(
   ("C-c r" . rename-file-and-buffer)
   ("C-c D" . delete-file-and-buffer)
   ("s-w" . toggle-window-split)
   ("M-0" . indent-buffer)
   ("C-x c u" . capitalize-one-char)
   ("C-x c l" . lowercase-one-char)
   ("C-x l" . mark-line)
   ("s-k" . kill-and-join-forward)      ;在缩进的行之间删除
   ("M-G" . goto-column)
   ("C-\"" . cursor-position-1-store)      ;; store cursor position
   ("C-:" . cursor-position-1-jump)       ;; jump to cursor position
   ("M-s-," . cursor-position-stack-pop)  ;; move corsor to the position popped from cursor-position-stack
   ("M-s-." . cursor-position-stack-push) ;; push cursor position to cursor-position-stack
   ("C-c g t" . goto-percent-text)
   ("C-c g l" . goto-percent-line)
   ("s-j" . scroll-up-one-line)
   ("s-k" . scroll-down-one-line)
   ("<f2>" . revert-buffer-no-confirm)  ;; reload file without confirm
   ("s-f" . find-file-root)             ;; open file in root
   ("s-r" . find-file-smb)              ;; visit sambao
   ("C-c C-t" . toggle-fold-by-indent)
   )
 "basic-toolkit")
;; --- delete-block
;; - 向前向后删除
(lazy-load-global-keys
 '(
   ("M-N" . delete-block-backward)
   ("M-M" . delete-block-forward)
   )
 "delete-block")
;; --- echo-keys
;; - 显示按键记录
(lazy-load-global-keys
 '(
   ("C-c e e" . toggle-echo-keys)
   ("C-c e c" . echo-keys-clean)
   )
 "echo-keys")
;; --- evals
(lazy-load-global-keys
 '(
   ("C-x e" . lsz-eval-elisp-to-next-line)
   )
 "evals")
;; --- force-indent
;; - 手动控制四空格缩进
(lazy-load-global-keys
 '(
   ("C-<" . lsz-un-indent)
   ("C->" . lsz-indent)
   )
 "force-indent")
;; --- goto-last-change
;; - 定位光标到最后编辑的地方
(lazy-load-global-keys
 '(
   ("C-\\" . goto-last-change)
   )
 "goto-last-change")
;; --- goto-line-preview
(lazy-load-global-keys
 '(
   ("M-g r" . goto-line-preview)
   )
 "goto-line-preview")
;; --- highlight-indentation
(lazy-load-global-keys
 '(
   ("C-c h h" . highlight-indentation-mode)
   ("C-c h c" . highlight-indentation-current-column-mode)
   )
 "highlight-indentation")
;; --- move-and-duplicate-line
(lazy-load-global-keys
 '(
   ("M-p" . move-text-up)
   ("M-n" . move-text-down)
   ("C-c d" . duplicate-current-line-or-region)
   ("C-c D" . duplicate-and-comment-current-line-or-region)
   )
 "move-and-duplicate-text")
;; --- toggle-one-window
;; - 临时最大化当前窗口
(lazy-load-global-keys
 '(
   ("M-s-o" . toggle-one-window)
   )
 "toggle-one-window")

;; ------ watch-next-window
;; "up" to see previous content
;; "down" to see further content
(lazy-load-global-keys
 '(
   ("M-j" . watch-next-window-up-line)
   ("M-k" . watch-next-window-down-line)
   ("M-J" . watch-next-window-up)
   ("M-K" . watch-next-window-down)
   )
 "watch-next-window")



;;; ------------ shortcut on functions in init-*.el

;; ------ init-session
(lazy-load-set-keys
 '(
   ("<f5>" . emacs-session-save)
   ))



;;; ------------ shortcut on built-in function

;; ------ org related
(lazy-load-set-keys
 '(
   ("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   ))

;; ------ move cursors in current buffer
(lazy-load-set-keys
 '(
   ("M-g k" . beginning-of-buffer)
   ("M-g j" . end-of-buffer)
   ("M-g f" . forward-paragraph)
   ("M-g b" . backward-paragraph)
   ("M-g y" . backward-up-list)         ;向左跳出 LIST
   ("M-g o" . up-list)                  ;向右跳出 LIST
   ("M-g u" . backward-down-list)       ;向左跳进 LIST
   ("M-g i" . down-list)                ;向右跳进 LIST
   ("M-g a" . beginning-of-defun)       ;函数开头
   ("M-g e" . end-of-defun)             ;函数末尾
   ))

;; ------ other
(lazy-load-set-keys
 '(
   ("C-z r" . global-hl-line-mode)      ;; toggle global-hl-line-mode
   ("C-z l" . display-line-numbers-mode)
   ("s--" . text-scale-decrease)    ;; smaller fonts
   ("s-=" . text-scale-increase)    ;; bigger fonts
   ("s-," . bury-buffer)
   ("s-." . unbury-buffer)
   ("C-s-q" . quoted-insert)        ;; read a input from keyboard and insert
   ("M-h" . set-mark-command) ;Instead C-Space for Chinese input method
   ("M-;" . comment-dwim)
   ))



(provide 'init-shortcut)

;;; init-shortcut.el ends here
