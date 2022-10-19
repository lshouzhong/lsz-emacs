;; -*- coding: utf-8; -*-
;;; Require:
(require 'js)
(require 'json-mode)
(require 'json-snatcher)

;;; Code:
(json-mode)

(defun lsz-jsons-print-path ()
  "Sets a hotkey for using the json-snatcher plugin"
  (when (string-match  "\\.json$" (buffer-name))
    (message (jsons-print-path))))

(lazy-load-set-keys
 '(
   ("C-c C-p" . lsz-jsons-print-path)
   )
 json-mode-map)

(provide 'init-json)

;;; init-json.el
