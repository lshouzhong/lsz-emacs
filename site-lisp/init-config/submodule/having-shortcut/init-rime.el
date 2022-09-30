;; -*- coding: utf-8; -*-
;;; Require:
(require 'rime)

;;; Code:

(setq default-input-method "rime")

(if window-system
    (setq rime-show-candidate 'posframe)
  (setq rime-show-candidate 'message)
)

;; set 'rime-share-data-dir' and 'rime-user-data-dir'
(when *win64*
  (setq rime-share-data-dir "~/Rime"))

(when *linux*
  (when (file-exists-p "~/.config/fcitx/rime")
    (setq rime-share-data-dir "~/.config/fcitx/rime"))
  (when (file-exists-p "~/.config/ibus/rime")
    (setq rime-share-data-dir "~/.config/ibus/rime")))

;; auto-disable
(setq rime-disable-predicates
      '(rime-predicate-prog-in-code-p
      rime-predicate-after-alphabet-char-p
      rime-predicate-ace-window-p))

;; (setq rime-inline-ascii-trigger 'shift-l)
;; (setq rime-inline-predicates
;;       '(rime-predicate-space-after-cc-p
;;       ))

;; rime posframe box theme
(setq rime-posframe-properties
      (list :background-color "#333333"
            :foreground-color "#dcdccc"
            :internal-border-width 6
            ;; :font "sarasa mono sc"
            ))
;; (set-face-attribute 'rime-default-face       nil :foreground "#81a1c1" :background "#3d424d")
;; (set-face-attribute 'rime-code-face          nil :foreground "#5e81ac" :background nil)
;; (set-face-attribute 'rime-candidate-num-face nil :foreground "#5e81ac" :background nil)
;; (set-face-attribute 'rime-comment-face       nil :foreground "#8fbcbb" :background nil)
(set-face-attribute 'rime-highlight-candidate-face nil :foreground "#008800" :background nil)

(defun lsz-rime-convert-string-at-point (&optional return-cregexp)
  "input the string before the cursor to rime."
  (interactive "P")
  (let ((string (if mark-active
                    (buffer-substring-no-properties
                      (region-beginning) (region-end))
                  (buffer-substring-no-properties
                    (point) (max (line-beginning-position) (- (point) 80)))))
        code
        length)
    (cond ((string-match "\\([a-z'-]+\\|[[:punct:]]\\) *$" string)
            (setq code (replace-regexp-in-string
                        "^[-']" ""
                        (match-string 0 string)))
            (setq length (length code))
            (setq code (replace-regexp-in-string " +" "" code))
            (if mark-active
                (delete-region (region-beginning) (region-end))
              (when (> length 0)
                (delete-char (- 0 length))))
            (when (> length 0)
              (setq unread-command-events
                    (append (listify-key-sequence code)
                            unread-command-events))))
          (t (message "`lsz-rime-convert-string-at-point' did nothing.")))))

(lazy-load-set-keys
 '(
   ("C-~" . rime-inline-ascii)
   ("C-M-`" . rime-force-enable)
   ("C-`" . rime-send-keybinding)
   ("C-c i" . lsz-rime-convert-string-at-point)
   )
 rime-mode-map)

(provide 'init-rime)

;;; init-rime.el ends here
