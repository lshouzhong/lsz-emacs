;; -*- coding: utf-8; -*-
;;; Require:
(require 'dired-display-buffer)
(require 'dired-narrow)
(require 'dired-subtree)

;;; Code:

(setq dired-listing-switches "-alh1v --group-directories-first")

;; dired-subtree
(setq dired-subtree-line-prefix "    ")
(setq dired-subtree-cycle-depth 3) ;; default `3'
(setq dired-subtree-use-backgrounds nil) ;; default `t'
;; (set-face-attribute 'dired-subtree-depth-1-face nil :background "#ced9db")
;; (set-face-attribute 'dired-subtree-depth-2-face nil :background "#bbc9cc")
;; (set-face-attribute 'dired-subtree-depth-3-face nil :background "#a7babe")
;; (set-face-attribute 'dired-subtree-depth-4-face nil :background "#94aaaf")
;; (set-face-attribute 'dired-subtree-depth-5-face nil :background "#809ba2")
;; (set-face-attribute 'dired-subtree-depth-6-face nil :background "#6c8b93")

(lazy-load-set-keys
 '(
   ;; dired-display-buffer
   ("o" . dired-display-buffer)
   ;; dired-narrow
   ("/" . dired-narrow)
   ;; dired-subtree
   ("<tab>" . dired-subtree-cycle)
   ("SPC" . dired-subtree-toggle)
   ("r" . dired-subtree-revert)
   ("C-p" . dired-subtree-previous-sibling)
   ("C-n" . dired-subtree-next-sibling)
   )
 dired-mode-map)

(provide 'init-dired)

;;; init-dired.el ends here
