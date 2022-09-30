;;; lsz-dark-theme.el --- Custom theme inspired by the FlatUI palette

;; Copyright (C) 2018 MetroWind.

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
;; file provides dark variant.

;;; Code:

;; Note: for every face that is customized here, a customization for
;; it should be also provided in the light version. Otherwise it could
;; be ugly when switching bwteen styles

(deftheme lsz-dark
  "Inspired by the color scheme from flatuicolors.com.")

;; Colors
(let*
    ((sz-turquoise "#1abc9c")
     (sz-emerald "#2ecc71")
     (sz-river "#3498db")
     (sz-amethyst "#9b59b6")
     (sz-deep-asphalt "#34495e")
     (sz-asphalt "#425d78")
     (sz-sunflower "#f1c40f")
     (sz-carrot "#e67e22")
     (sz-alizarin "#e74c3c")
     (sz-clouds "#ecf0f1")
     (sz-concrete "#95a5a6")
     (sz-dark-turquoise "#16a085")
     (sz-dark-emerald "#27ae60")
     (sz-dark-river "#2980b9")
     (sz-dark-amethyst "#8e44ad")
     (sz-dark-asphalt "#2c3e50")
     (sz-dark-sunflower "#f39c12")
     (sz-dark-carrot "#d35400")
     (sz-dark-alizarin "#c0392b")
     (sz-dark-clouds "#bdc3c7")
     (sz-deep-clouds "#dce0e1")
     (sz-dark-concrete "#7f8c8d")

     (sz-bg sz-dark-asphalt)
     (sz-fg sz-dark-clouds))

  (custom-theme-set-faces
   'lsz-dark
   `(default ((t (:background ,sz-bg
                  :foreground ,sz-fg))))
   `(cursor ((t (:background ,sz-carrot
                 :foreground ,sz-fg))))
   `(region ((t (:background ,sz-dark-sunflower
                 :foreground ,sz-bg))))
   `(mode-line ((t (:background ,sz-asphalt
                    :foreground ,sz-fg
                    :box nil))))
   `(mode-line-buffer-id ((t (:foreground ,sz-fg))))
   `(mode-line-inactive ((t (:background ,sz-bg
                             :foreground ,sz-fg))))
   `(fringe ((t (:background ,sz-bg))))
   `(minibuffer-prompt ((t (:inherit italic :foreground ,sz-dark-concrete))))
   `(font-lock-builtin-face ((t (:foreground ,sz-deep-clouds))))
   `(font-lock-comment-face ((t (:inherit italic :foreground ,sz-dark-concrete))))
   `(font-lock-constant-face ((t (:inherit italic :foreground ,sz-deep-clouds))))
   `(font-lock-function-name-face ((t (:foreground ,sz-sunflower))))
   `(font-lock-keyword-face ((t (:foreground ,sz-deep-clouds :inherit italic))))
   `(font-lock-string-face ((t (:foreground ,sz-turquoise))))
   `(font-lock-type-face ((t (:foreground ,sz-emerald))))
   `(font-lock-variable-name-face ((t (:foreground ,sz-river))))
   `(font-lock-warning-face ((t (:foreground ,sz-dark-carrot))))
   `(isearch ((t (:background ,sz-dark-concrete
                  :foreground ,sz-fg))))
   `(lazy-highlight ((t (:background ,sz-concrete))))
   `(link ((t (:foreground ,sz-river :underline t))))
   `(link-visited ((t (:foreground ,sz-dark-concrete :underline t))))
   `(button ((t (:background ,sz-bg :underline t :foreground nil))))
   `(header-line ((t (:background ,sz-asphalt
                      :foreground ,sz-fg))))
   `(shadow ((t (:foreground ,sz-dark-concrete))))
   `(show-paren-match ((t (:background ,sz-emerald :foreground ,sz-bg))))
   `(show-paren-mismatch ((t (:background ,sz-alizarin
                              :foreground ,sz-bg))))
   `(highlight ((t (:inverse-video nil :background ,sz-asphalt))))
   `(hl-line ((t (:inverse-video nil :background ,sz-asphalt))))
   `(widget-field ((t (:background ,sz-dark-concrete))))

   ;; Face for specific prog modes
   `(sh-heredoc ((t (:foreground nil :inherit font-lock-string-face))))

   ;; Dired
   `(dired-directory ((t (:foreground ,sz-river))))
   `(dired-symlink ((t (:foreground ,sz-turquoise))))
   `(dired-perm-write ((t (:foreground ,sz-dark-carrot))))

   ;; Diff
   `(diff-added ((t (:foreground ,sz-turquoise))))
   `(diff-removed ((t (:foreground ,sz-alizarin))))
   ;; `(diff-context ((t (:inherit default))))
   `(diff-file-header ((t (:bold t :background ,sz-asphalt :weight bold :foreground ,sz-clouds))))
   `(diff-header ((t (:foreground ,sz-concrete :background ,sz-bg))))

   ;; Whitespace
   `(whitespace-trailing ((t (:background ,sz-asphalt))))
   `(whitespace-line ((t (:background ,sz-asphalt :foreground ,sz-fg))))

   ;; ERC
   `(erc-notice-face ((t (:foreground ,sz-river
                          :weight unspecified))))
   `(erc-header-line ((t (:background ,sz-asphalt))))
   `(erc-timestamp-face ((t (:foreground ,sz-dark-concrete
                             :weight unspecified))))
   `(erc-current-nick-face ((t (:foreground ,sz-sunflower
                                :weight unspecified))))
   `(erc-input-face ((t (:foreground ,sz-fg))))
   `(erc-prompt-face ((t (:foreground ,sz-dark-concrete
                          :background nil
                          :inherit italic
                          :weight unspecified))))
   `(erc-my-nick-face ((t (:foreground ,sz-sunflower))))
   `(erc-pal-face ((t (:foreground ,sz-emerald))))

   ;; Rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,sz-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,sz-river))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,sz-carrot))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,sz-amethyst))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,sz-sunflower))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,sz-emerald))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,sz-concrete))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,sz-alizarin))))

   ;; Magit
   `(magit-branch-local ((t (:foreground ,sz-river :background nil))))
   `(magit-branch-remote ((t (:foreground ,sz-emerald :background nil))))
   `(magit-tag ((t (:foreground ,sz-river :background ,sz-bg))))
   `(magit-hash ((t (:foreground ,sz-concrete))))
   `(magit-section-title ((t (:foreground ,sz-dark-emerald :background ,sz-bg))))
   `(magit-section-heading ((t (:background ,sz-asphalt :foreground ,sz-fg))))
   `(magit-section-highlight ((t (:background ,sz-asphalt))))
   `(magit-item-highlight ((t (:foreground ,sz-fg :background ,sz-deep-clouds))))
   `(magit-log-author ((t (:foreground ,sz-sunflower))))
   `(magit-diff-added ((t (:inherit diff-added))))
   `(magit-diff-added-highlight ((t (:inherit magit-diff-added))))
   `(magit-diff-removed ((t (:inherit diff-removed))))
   `(magit-diff-removed-highlight ((t (:inherit magit-diff-removed))))
   `(magit-diff-context ((t (:inherit diff-context))))
   `(magit-diff-context-highlight ((t (:inherit magit-diff-context))))

   ;; Git-gutter-fringe
   `(git-gutter-fr:modified ((t (:foreground ,sz-sunflower))))
   `(git-gutter-fr:added ((t (:foreground ,sz-emerald))))
   `(git-gutter-fr:deleted ((t (:foreground ,sz-alizarin))))

   ;; Company
   `(company-preview ((t (:foreground ,sz-fg :background ,sz-sunflower))))
   `(company-preview-common ((t (:foreground ,sz-fg :background ,sz-carrot))))
   `(company-tooltip ((t (:foreground ,sz-fg :background ,sz-asphalt))))
   `(company-tooltip-common ((t (:foreground ,sz-sunflower))))
   `(company-tooltip-selection ((t (:background ,sz-deep-asphalt))))
   `(company-tooltip-common-selection ((t (:foreground ,sz-sunflower))))
   `(company-tooltip-annotation ((t (:foreground ,sz-emerald))))
   `(company-scrollbar-bg ((t (:background ,sz-bg))))
   `(company-scrollbar-fg ((t (:background ,sz-asphalt))))

   ;; Cperl
   `(cperl-array-face ((t (:weight bold :inherit font-lock-variable-name-face))))
   `(cperl-hash-face ((t (:weight bold :inherit (italic font-lock-variable-name-face)))))
   `(cperl-nonoverridable-face ((t (:inherit font-lock-builtin-face))))

   ;; Powerline
   `(mode-line ((t (:box nil))))
   `(powerline-active2 ((t (:foreground ,sz-fg :background ,sz-dark-clouds))))
   `(powerline-active1 ((t (:foreground ,sz-bg :background ,sz-emerald))))
   `(powerline-inactive2 ((t (:foreground ,sz-bg :background ,sz-concrete))))
   `(powerline-inactive1 ((t (:foreground ,sz-fg :background ,sz-dark-clouds))))

   ;; Smart mode line
   `(sml/global  ((t (:foreground ,sz-fg))))
   `(sml/charging ((t (:foreground ,sz-emerald))))
   `(sml/discharging ((t (:foreground ,sz-dark-alizarin))))
   `(sml/read-only ((t (:foreground ,sz-dark-emerald))))
   `(sml/filename ((t (:foreground ,sz-turquoise))))
   `(sml/prefix ((t (:foreground ,sz-amethyst :weight normal :inherit italic))))
   `(sml/modes ((t (:foreground ,sz-fg :weight bold))))
   `(sml/modified ((t (:foreground ,sz-alizarin))))
   `(sml/outside-modified ((t (:foreground ,sz-bg :background ,sz-alizarin))))
   `(sml/position-percentage ((t (:foreground ,sz-river :slant normal))))

   ;; Helm
   `(helm-candidate-number ((t (:foreground ,sz-fg :background nil))))
   `(helm-source-header ((t (:foreground ,sz-fg :background ,sz-asphalt
                                         :weight normal :inherit italic))))
   `(helm-selection ((t (:background ,sz-dark-sunflower :foreground ,sz-bg))))
   `(helm-prefarg ((t (:foreground ,sz-dark-alizarin))))
   `(helm-ff-directory ((t (:foreground ,sz-river))))
   `(helm-ff-executable ((t (:foreground ,sz-emerald))))
   `(helm-ff-invalid-symlink ((t (:foreground ,sz-bg :background ,sz-dark-alizarin))))
   `(helm-ff-symlink ((t (:foreground ,sz-amethyst))))
   `(helm-ff-prefix ((t (:background ,sz-sunflower))))
   `(helm-ff-dotted-directory ((t (:background nil :foreground ,sz-dark-concrete))))
   `(helm-M-x-key ((t (:foreground ,sz-dark-emerald))))
   `(helm-buffer-file ((t (:foreground ,sz-fg))))
   `(helm-buffer-archive ((t (:inherit helm-buffer-file))))
   `(helm-buffer-directory ((t (:foreground ,sz-river :background nil))))
   `(helm-buffer-not-saved ((t (:foreground ,sz-dark-alizarin))))
   `(helm-buffer-modified ((t (:foreground ,sz-carrot))))
   `(helm-buffer-process ((t (:foreground ,sz-dark-emerald))))
   `(helm-buffer-size ((t (:foreground ,sz-dark-concrete))))
   `(helm-ff-file ((t (:inherit default))))

   ;; TeX
   `(font-latex-sedate-face ((t (:foreground ,sz-river))))
   `(font-latex-math-face ((t (:foreground ,sz-turquoise))))
   `(font-latex-script-char-face ((t (:inherit font-latex-math-face))))

   ;; adoc-mode
   `(markup-meta-hide-face ((t (:height 1.0 :foreground ,sz-fg))))
   `(markup-meta-face ((t (:height 1.0 :foreground ,sz-fg :family nil))))
   `(markup-reference-face ((t (:underline nil :foreground ,sz-river))))
   `(markup-gen-face ((t (:foreground ,sz-emerald))))
   `(markup-passthrough-face ((t (:inherit markup-gen-face))))
   `(markup-replacement-face ((t (:family nil :foreground ,sz-amethyst))))
   `(markup-list-face ((t (:weight bold))))
   `(markup-secondary-text-face ((t (:height 1.0 :foreground ,sz-emerald))))
   `(markup-verbatim-face ((t (:foreground ,sz-concrete))))
   `(markup-typewriter-face ((t (:inherit nil))))
   `(markup-title-0-face ((t (:height 1.2 :inherit markup-gen-face))))
   `(markup-title-1-face ((t (:height 1.0 :inherit markup-gen-face))))
   `(markup-title-2-face ((t (:height 1.0 :inherit markup-gen-face))))
   `(markup-title-3-face ((t (:height 1.0 :inherit markup-gen-face))))
   `(markup-title-4-face ((t (:height 1.0 :inherit markup-gen-face))))
   `(markup-title-5-face ((t (:height 1.0 :inherit markup-gen-face))))

   ;; Org-mode
   `(org-hide ((t (:foreground ,sz-bg))))
   `(org-table ((t (:foreground ,sz-fg))))
   `(org-date ((t (:foreground ,sz-emerald))))
   `(org-done ((t (:weight normal :foreground ,sz-dark-concrete))))
   `(org-todo ((t (:weight normal :foreground ,sz-carrot))))
   `(org-latex-and-related ((t (:foreground ,sz-concrete :italic t))))
   `(org-checkbox ((t (:weight normal :foreground ,sz-concrete))))
   `(org-mode-line-clock ((t (:background nil))))
   `(org-document-title ((t (:weight normal :foreground nil))))

   ;; org-tree-slide
   `(org-tree-slide-header-overlay-face
     ((t (:inherit font-lock-comment-face :foreground nil :background nil))))

   ;; Message
   `(message-header-name ((t (:foreground ,sz-concrete))))
   `(message-header-other ((t (:foreground ,sz-fg))))
   `(message-header-cc ((t (:inherit message-header-other))))
   `(message-header-newsgroups ((t (:inherit message-header-other))))
   `(message-header-xheader ((t (:inherit message-header-other))))
   `(message-header-subject ((t (:foreground ,sz-emerald))))
   `(message-header-to ((t (:foreground ,sz-river))))
   `(message-mml ((t (:foreground ,sz-dark-concrete))))

   ;; Notmuch
   `(notmuch-search-unread-face ((t (:foreground ,sz-river))))
   `(notmuch-tag-face ((t (:foreground ,sz-emerald))))
   `(notmuch-tree-match-author-face ((t (:foreground ,sz-river))))
   `(notmuch-tree-no-match-face ((t (:foreground ,sz-concrete))))
   `(notmuch-tree-match-tag-face ((t (:inherit notmuch-tree-match-author-face))))
   `(notmuch-tag-unread-face ((t (:foreground ,sz-carrot))))
   `(notmuch-message-summary-face ((t (:foreground ,sz-concrete))))

   ;; Highlight-indent-guides
   `(highlight-indent-guides-odd-face ((t (:background ,sz-deep-asphalt))))
   `(highlight-indent-guides-even-face ((t (:background nil))))

   ;; Telega
   `(telega-msg-heading ((t (:background nil :foreground ,sz-emerald :inherit nil))))
   `(telega-msg-inline-reply ((t (:foreground ,sz-dark-concrete :inherit nil))))
   `(telega-entity-type-texturl ((t (:inherit nil :foreground ,sz-river))))
   ))

(provide-theme 'lsz-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; lsz-dark-theme.el ends here
