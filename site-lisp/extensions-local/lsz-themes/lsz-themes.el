;;; lsz-themes.el --- Custom theme inspired by the Flat UI palette
;;
;; Copyright (C) 2018 MetroWind.
;;
;; This program is free software. It comes without any warranty, to
;; the extent permitted by applicable law. You can redistribute it
;; and/or modify it under the terms of the Do What the Fuck You Want
;; to Public License, Version 2, as published by Sam Hocevar. See
;; http://www.wtfpl.net/ for more details.

;; Author: MetroWind <chris.corsair@gmail.com>
;; URL: https://github.com/MetroWind/lsz-theme
;; Keywords: lisp
;; Version: 1.0
;; Package-Requires: ((emacs "24"))

;;; Commentary:
;;
;; Fluc UI theme is a custom theme for Emacs, inspired by
;; http://flatuicolors.com. It Has both light and dark variants. This
;; file provides some utilities to load and switch themes.

;;; Code:
(defvar lsz-themes-current-style nil)

;;;###autoload
(add-to-list 'custom-theme-load-path
             (file-name-directory load-file-name))

;;;###autoload
(defun lsz-themes-load-style (style)
  "Load lsz theme variant STYLE.

Argument STYLE can be either 'light or 'dark."

  (interactive)
  (cond ((equal style 'light)
         (load-theme 'lsz-light t))
        ((equal style 'dark)
         (load-theme 'lsz-dark t))

        (t (error (format "Unknown lsz theme style: %S" style)))))

;;;###autoload
(defun lsz-themes-switch-style ()
  "Toggle between the light and dark style of lsz theme."
  (interactive)
  (cond ((or (null lsz-themes-current-style)
             (equal lsz-themes-current-style 'dark))
         (lsz-themes-load-style 'light)
         (setq lsz-themes-current-style 'light))
        ((equal lsz-themes-current-style 'light)
         (lsz-themes-load-style 'dark)
         (setq lsz-themes-current-style 'dark))
        (t (error (format "Invalid lsz current style: %S"
                          lsz-themes-current-style)))))

(provide 'lsz-themes)

;;; FlatUI-theme-utils.el ends here
