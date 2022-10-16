;; -*- coding: utf-8; -*-
;;; Require:
(require 'org)
(require 'ox-publish)

;;; Code:

;; unbind keys
(define-key org-mode-map (kbd "C-,") nil)

;; turn on word-wrap in org-mode
(add-hook 'org-mode-hook 'visual-line-mode)

(defun lsz-org-buffer-update-modified-property ()
  "If '#+LASTUPDATE' is in org file, update it to the current date/time."
  (when (eq major-mode 'org-mode)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+LASTUPDATE:" (point-max) t)
        (progn
          (setq lastupdate-point (point))
          (if (not (equal lastupdate-point (line-end-position)))
              (delete-region lastupdate-point (line-end-position)))
          (insert (format-time-string " %Y/%m/%d %H:%M")))))))

(add-hook 'before-save-hook #'lsz-org-buffer-update-modified-property)

(setq org-startup-indented nil) ;; enable org-indent-mode at start, default nil.

(setq org-goto-auto-isearch nil)
(setq org-support-shift-select t)

(setq org-use-sub-superscripts '{})
(setq org-export-with-sub-superscripts '{})

;; (set-face-attribute 'org-level-1 nil
;;                     :background "#fdf0ff"
;;                     :foreground "#000000"
;;                     :overline "#bcbcbc"
;;                     :bold t
;;                     :height 1.3)
;; (set-face-attribute 'org-level-2 nil
;;                     :foreground "#8f0075"
;;                     :overline "#bcbcbc"
;;                     :bold t
;;                     :height 1.1)
;; (set-face-attribute 'org-level-3 nil
;;                     :weight 'semi-bold)
;; (set-face-attribute 'org-level-4 nil
;;                     :weight 'semi-bold)
;; (set-face-attribute 'org-level-5 nil
;;                     :weight 'semi-bold)
;; (set-face-attribute 'org-level-6 nil
;;                     :weight 'semi-bold)

(defun lsz-org-html-src-block-customized (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information.
MAKE FINAL HTML TO BE COMPATIBLE WITH highlight.js"
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let ((lang (org-element-property :language src-block))
          (caption (org-export-get-caption src-block))
          (code (org-html-format-code src-block info))
          (label (let ((lbl (and (org-element-property :name src-block)
                                 (org-export-get-reference src-block info))))
                   (if lbl (format " id=\"%s\"" lbl) ""))))
      (if (not lang) (format "<pre class=\"example\"%s>\n%s</pre>" label code)
        (format
         "<div class=\"org-src-container\">\n%s%s\n</div>"
         (if (not caption) ""
           (format "<label class=\"org-src-name\">%s</label>"
                   (org-export-data caption info)))
         (format "\n<pre class=\"src src-%s\"%s><code class=\"language-%s\">%s</code></pre>"
                 lang label lang code))))))

(advice-add 'org-html-src-block :override 'lsz-org-html-src-block-customized)

(defun lsz-org-html-format-list-item-customized (contents type checkbox info
                                                          &optional term-counter-id
                                                          headline)
  "Format a list item into HTML."
  (let ((class (if checkbox
                   (format " class=\"%s\""
                           (symbol-name checkbox)) ""))
        (checkbox (concat (org-html-checkbox checkbox info)
                          (and checkbox " ")))
        (br (org-html-close-tag "br" nil info))
        (extra-newline (if (and (org-string-nw-p contents) headline) "\n" "")))
    (concat
     (pcase type
       (`ordered
        (let* ((counter term-counter-id)
               (extra (if counter (format " value=\"%s\"" counter) "")))
          (concat
           (format "<li%s%s>" class extra)
           (when headline (concat headline br)))))
       (`unordered
        (let* ((id term-counter-id)
               (extra (if id (format " id=\"%s\"" id) "")))
          (concat
           (format "<li%s%s>" class extra)
           (when headline (concat headline br)))))
       (`descriptive
        (let* ((term term-counter-id))
          (setq term (or term "(no term)"))
          ;; Check-boxes in descriptive lists are associated to tag.
          (concat (format "<dt%s>%s</dt>"
                          class (concat checkbox term))
                  "<dd><p>"))))
     (unless (eq type 'descriptive) checkbox)
     extra-newline
     (and (org-string-nw-p contents) (org-trim contents))
     extra-newline
     (pcase type
       (`ordered "</li>")
       (`unordered "</li>")
       (`descriptive "</p></dd>")))))

(advice-add 'org-html-format-list-item :override 'lsz-org-html-format-list-item-customized)

(setq org-export-with-sub-superscripts '{})

(defun lsz-org-export-publish()
  "Publish org and then generate sitemap.xml file."
  (interactive)
  ;; import project settings
  (require 'org-publish-project-desc)
  ;; publish site
  (org-publish-all)
  ;; create sitemap for search engine
  (let (
        ;; FULL PATH to doc root. Must end in a slash. Must not start with ~
        (lsz-site-pub-path-article-root "~/Public/lsz_org_article_publish/article/")
        ;; file name of sitemap file, relative to webroot.
        ;; file name format: <sitemap-file-name>.xml
        (lsz-site-sitemap-file-name "sitemap.xml")
        ;; site domain name
        (lsz-site-domain-name "lishouzhong.com")
        ;; gzip it or not. t for true, nil for false.
        (lsz-site-sitemap-gzip-it-p nil))

    (print (concat "begin: " (format-time-string "%Y-%m-%dT%T")))

    ;; rename file to backup ~ if already exist
    (let (f1 f2)
      (setq f1 (concat lsz-site-pub-path-article-root lsz-site-sitemap-file-name))
      (setq f2 (concat f1 ".gz"))
      (when (file-exists-p f1)
        (rename-file f1 (concat f1 "~") t))
      (when (file-exists-p f2)
        (rename-file f2 (concat f2 "~") t)))

    ;; create sitemap buffer
    (let (article-file-path sitemap-buffer)
      (setq article-file-path
            (concat lsz-site-pub-path-article-root lsz-site-sitemap-file-name))
      (setq sitemap-buffer (find-file article-file-path))
      (erase-buffer)
      (set-buffer-file-coding-system 'unix)
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">")

      (require 'find-lisp)

      (let ((process-sitemap-content
             (lambda (article-file-path dest-buffer)
               (when (not (string-match "/zzz" article-file-path)) ; dir/file starting with zzz are not public
                 (with-temp-buffer
                   (insert-file-contents article-file-path nil nil nil t)
                   (goto-char 1)
                   (when (not (search-forward "<meta http-equiv=\"refresh\"" nil "noerror"))
                     (with-current-buffer dest-buffer
                       (insert "<url><loc>")
                       (insert (concat
                                "http://"
                                lsz-site-domain-name
                                "/"
                                (substring article-file-path
                                           (length (expand-file-name lsz-site-pub-path-article-root)))))
                       (insert "</loc></url>\n"))))))))
        (mapc
         (lambda (x) (funcall process-sitemap-content x sitemap-buffer))
         (find-lisp-find-files lsz-site-pub-path-article-root "\\.html$")))

      (insert "</urlset>")

      (save-buffer)

      (when lsz-site-sitemap-gzip-it-p
        (shell-command (concat "gzip " article-file-path))))

    (print (concat "finished: " (format-time-string "%Y-%m-%dT%T")))))

(provide 'init-org)

;;; init-org.el ends here
