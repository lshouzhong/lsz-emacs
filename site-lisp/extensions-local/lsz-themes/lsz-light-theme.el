;;; lsz-light-theme.el --- Custom theme inspired by the FlatUI palette

;; Copyright (C) 2010--2018 MetroWind.

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
;; file provides light variant.

;;; Code:

;; Note: for every face that is customized here, a customization for
;; it should be also provided in the dark version. Otherwise it could
;; be ugly when switching bwteen styles

(deftheme lsz-light
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
    ;;  (sz-concrete "#95a5a6")
     (sz-concrete "#7f8c8d")
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
    ;;  (sz-dark-concrete "#7f8c8d")
     (sz-dark-concrete "#95a5a6")

     (sz-bg sz-clouds)
     (sz-fg sz-asphalt))

  (custom-theme-set-faces
   'lsz-light
   `(default ((t (:background ,sz-bg
                  :foreground ,sz-fg))))
   `(cursor ((t (:background ,sz-carrot
                 :foreground ,sz-fg))))
   `(region ((t (:background ,sz-dark-sunflower
                 :foreground ,sz-fg))))
   `(mode-line ((t (:background ,sz-deep-clouds
                    :foreground ,sz-fg
                    :box nil))))
   `(mode-line-buffer-id ((t (:foreground ,sz-fg))))
   `(mode-line-inactive ((t (:background ,sz-dark-clouds
                             :foreground ,sz-fg))))
   `(fringe ((t (:background ,sz-bg))))
   `(minibuffer-prompt ((t (:inherit italic :foreground ,sz-dark-concrete))))
   `(font-lock-builtin-face ((t (:foreground ,sz-dark-asphalt))))
   `(font-lock-comment-face ((t (:inherit italic :foreground ,sz-concrete))))
   `(font-lock-constant-face ((t (:inherit italic :foreground ,sz-dark-concrete))))
   `(font-lock-function-name-face ((t (:foreground ,sz-amethyst))))
   `(font-lock-keyword-face ((t (:foreground ,sz-dark-asphalt :inherit italic))))
   `(font-lock-string-face ((t (:foreground ,sz-dark-turquoise))))
   `(font-lock-type-face ((t (:foreground ,sz-dark-emerald))))
   `(font-lock-variable-name-face ((t (:foreground ,sz-river))))
   `(font-lock-warning-face ((t (:foreground ,sz-dark-carrot))))
   `(isearch ((t (:background ,sz-dark-concrete
                  :foreground ,sz-fg))))
   `(lazy-highlight ((t (:background ,sz-concrete))))
   `(link ((t (:foreground ,sz-dark-river :underline t))))
   `(link-visited ((t (:foreground ,sz-dark-asphalt :underline t))))
   `(button ((t (:background ,sz-carrot :underline t :foreground nil))))
   `(header-line ((t (:background ,sz-deep-clouds
                      :foreground ,sz-fg))))
   `(shadow ((t (:foreground ,sz-concrete))))
   `(show-paren-match ((t (:background ,sz-emerald :foreground ,sz-clouds))))
   `(show-paren-mismatch ((t (:background ,sz-alizarin
                              :foreground ,sz-clouds))))
   `(highlight ((t (:inverse-video nil :background ,sz-deep-clouds))))
   `(hl-line ((t (:inverse-video nil :background ,sz-deep-clouds))))
   `(widget-field ((t (:background ,sz-concrete))))

   ;; Face for specific prog modes
   `(sh-heredoc ((t (:foreground nil :inherit font-lock-string-face))))

   ;; Dired
   `(dired-directory ((t (:foreground ,sz-river))))
   `(dired-symlink ((t (:foreground ,sz-dark-turquoise))))
   `(dired-perm-write ((t (:foreground ,sz-dark-carrot))))

   ;; Diff
   `(diff-added ((t (:foreground ,sz-river))))
   `(diff-removed ((t (:foreground ,sz-alizarin))))
   ;; `(diff-context ((t (:background nil))))
   `(diff-file-header ((t (:bold t :background ,sz-concrete :weight bold))))
   `(diff-header ((t (:background ,sz-deep-clouds :foreground ,sz-fg))))

   ;; Whitespace
   `(whitespace-trailing ((t (:background ,sz-dark-clouds))))
   `(whitespace-line ((t (:background ,sz-dark-clouds :foreground unspecified))))

   ;; ERC
   `(erc-notice-face ((t (:foreground ,sz-dark-river
                          :weight unspecified))))
   `(erc-header-line ((t (:foreground ,sz-bg :background ,sz-dark-clouds))))
   `(erc-timestamp-face ((t (:foreground ,sz-concrete
                             :weight unspecified))))
   `(erc-current-nick-face ((t (:foreground ,sz-dark-carrot
                                :weight unspecified))))
   `(erc-input-face ((t (:foreground ,sz-amethyst))))
   `(erc-prompt-face ((t (:foreground ,sz-dark-concrete
                          :background nil
                          :inherit italic
                          :weight unspecified))))
   `(erc-my-nick-face ((t (:foreground ,sz-dark-carrot))))
   `(erc-pal-face ((t (:foreground ,sz-dark-amethyst))))

   ;; Rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,sz-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,sz-turquoise))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,sz-dark-river))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,sz-dark-amethyst))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,sz-dark-sunflower))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,sz-dark-emerald))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,sz-dark-concrete))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,sz-alizarin))))

   ;; Magit
   `(magit-branch-local ((t (:foreground ,sz-river :background nil))))
   `(magit-branch-remote ((t (:foreground ,sz-dark-emerald :background nil))))
   `(magit-tag ((t (:foreground ,sz-river :background ,sz-bg))))
   `(magit-hash ((t (:foreground ,sz-concrete))))
   `(magit-section-title ((t (:foreground ,sz-dark-emerald :background ,sz-bg))))
   `(magit-section-heading ((t (:background ,sz-bg :foreground ,sz-fg))))
   `(magit-section-highlight ((t (:background ,sz-bg))))
   `(magit-item-highlight ((t (:foreground ,sz-fg :background ,sz-dark-clouds))))
   `(magit-log-author ((t (:foreground ,sz-amethyst))))
   `(magit-diff-added ((t (:inherit diff-added))))
   `(magit-diff-added-highlight ((t (:inherit magit-diff-added))))
   `(magit-diff-removed ((t (:inherit diff-removed))))
   `(magit-diff-removed-highlight ((t (:inherit magit-diff-removed))))
   `(magit-diff-context ((t (:inherit diff-context))))
   `(magit-diff-context-highlight ((t (:inherit magit-diff-context))))

   ;; Git-gutter-fringe
   `(git-gutter-fr:modified ((t (:foreground ,sz-amethyst))))
   `(git-gutter-fr:added ((t (:foreground ,sz-emerald))))
   `(git-gutter-fr:deleted ((t (:foreground ,sz-alizarin))))

   ;; Company
   `(company-preview ((t (:foreground ,sz-fg :background ,sz-sunflower))))
   `(company-preview-common ((t (:foreground ,sz-fg :background ,sz-carrot))))
   `(company-tooltip ((t (:foreground ,sz-fg :background ,sz-dark-clouds))))
   `(company-tooltip-common ((t (:foreground ,sz-dark-carrot))))
   `(company-tooltip-selection ((t (:background ,sz-deep-clouds))))
   `(company-tooltip-common-selection ((t (:foreground ,sz-dark-carrot))))
   `(company-tooltip-annotation ((t (:foreground ,sz-river))))
   `(company-scrollbar-bg ((t (:background ,sz-bg))))
   `(company-scrollbar-fg ((t (:background ,sz-dark-clouds))))

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
   `(sml/filename ((t (:foreground ,sz-river :weight bold))))
   `(sml/prefix ((t (:foreground ,sz-dark-amethyst :weight normal :inherit italic))))
   `(sml/modes ((t (:foreground ,sz-fg :weight bold))))
   `(sml/modified ((t (:foreground ,sz-alizarin))))
   `(sml/outside-modified ((t (:foreground ,sz-bg :background ,sz-alizarin))))
   `(sml/position-percentage ((t (:foreground ,sz-amethyst :slant normal))))

   ;; Helm
   `(helm-candidate-number ((t (:foreground ,sz-fg :background nil))))
   `(helm-source-header ((t (:foreground ,sz-bg :background ,sz-river
                                         :weight normal :inherit italic))))
   `(helm-selection ((t (:background ,sz-dark-sunflower))))
   `(helm-prefarg ((t (:foreground ,sz-dark-alizarin))))
   `(helm-ff-directory ((t (:foreground ,sz-river))))
   `(helm-ff-executable ((t (:foreground ,sz-dark-emerald))))
   `(helm-ff-invalid-symlink ((t (:foreground ,sz-bg :background ,sz-dark-alizarin))))
   `(helm-ff-symlink ((t (:foreground ,sz-amethyst))))
   `(helm-ff-prefix ((t (:background ,sz-sunflower))))
   `(helm-ff-dotted-directory ((t (:background nil :foreground ,sz-dark-clouds))))
   `(helm-M-x-key ((t (:foreground ,sz-dark-emerald))))
   `(helm-buffer-file ((t (:foreground ,sz-fg))))
   `(helm-buffer-archive ((t (:inherit helm-buffer-file))))
   `(helm-buffer-directory ((t (:foreground ,sz-river :background nil))))
   `(helm-buffer-not-saved ((t (:foreground ,sz-dark-alizarin))))
   `(helm-buffer-modified ((t (:foreground ,sz-carrot))))
   `(helm-buffer-process ((t (:foreground ,sz-dark-emerald))))
   `(helm-buffer-size ((t (:foreground ,sz-concrete))))
   `(helm-ff-file ((t (:inherit default))))

   ;; TeX
   `(font-latex-sedate-face ((t (:foreground ,sz-river))))
   `(font-latex-math-face ((t (:foreground ,sz-dark-turquoise))))
   `(font-latex-script-char-face ((t (:inherit font-latex-math-face))))

   ;; adoc-mode
   `(markup-meta-hide-face ((t (:height 1.0 :foreground ,sz-fg))))
   `(markup-meta-face ((t (:height 1.0 :foreground ,sz-fg :family nil))))
   `(markup-reference-face ((t (:underline nil :foreground ,sz-dark-river))))
   `(markup-gen-face ((t (:foreground ,sz-dark-river))))
   `(markup-passthrough-face ((t (:inherit markup-gen-face))))
   `(markup-replacement-face ((t (:family nil :foreground ,sz-amethyst))))
   `(markup-list-face ((t (:weight bold))))
   `(markup-secondary-text-face ((t (:height 1.0 :foreground ,sz-dark-emerald))))
   `(markup-verbatim-face ((t (:foreground ,sz-dark-concrete))))
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
   `(org-checkbox ((t (:weight normal :foreground ,sz-dark-concrete))))
   `(org-mode-line-clock ((t (:background nil))))
   `(org-document-title ((t (:weight normal :foreground nil))))

   ;; org-tree-slide
   `(org-tree-slide-header-overlay-face
     ((t (:inherit font-lock-comment-face :foreground nil :background nil))))

   ;; Message
   `(message-header-name ((t (:foreground ,sz-dark-concrete))))
   `(message-header-other ((t (:foreground ,sz-fg))))
   `(message-header-cc ((t (:inherit message-header-other))))
   `(message-header-newsgroups ((t (:inherit message-header-other))))
   `(message-header-xheader ((t (:inherit message-header-other))))
   `(message-header-subject ((t (:foreground ,sz-dark-emerald))))
   `(message-header-to ((t (:foreground ,sz-dark-river))))
   `(message-mml ((t (:foreground ,sz-concrete))))

   ;; Notmuch
   `(notmuch-search-unread-face ((t (:foreground ,sz-dark-river))))
   `(notmuch-tag-face ((t (:foreground ,sz-dark-emerald))))
   `(notmuch-tree-match-author-face ((t (:foreground ,sz-dark-river))))
   `(notmuch-tree-no-match-face ((t (:foreground ,sz-concrete))))
   `(notmuch-tree-match-tag-face ((t (:inherit notmuch-tree-match-author-face))))
   `(notmuch-tag-unread-face ((t (:foreground ,sz-carrot))))
   `(notmuch-message-summary-face ((t (:foreground ,sz-dark-concrete))))

   ;; Highlight-indent-guides
   `(highlight-indent-guides-odd-face ((t (:background ,sz-deep-clouds))))
   `(highlight-indent-guides-even-face ((t (:background nil))))

   ;; Telega
   `(telega-msg-heading ((t (:background nil :foreground ,sz-dark-emerald :inherit nil))))
   `(telega-msg-inline-reply ((t (:foreground ,sz-concrete :inherit nil))))
   `(telega-entity-type-texturl ((t (:inherit nil :foreground ,sz-dark-river))))
  ))

(provide-theme 'lsz-light)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; lsz-light-theme.el ends here
