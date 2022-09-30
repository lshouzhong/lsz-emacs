;; -*- coding: utf-8; -*-
;;; Require:

;;; Code:
(setq
 org-publish-project-alist
 (let* ((lsz-site-path "~/Documents/lsz_org_article/")
        (lsz-site-pub-path "~/Public/lsz_org_article_publish/")
        (get-content (lambda (x)
                       (with-temp-buffer
                         (insert-file-contents (concat lsz-site-path x))
                         (buffer-string))))
        (lsz-site-postamble (funcall get-content "template/postamble.html"))
        (lsz-site-preamble (funcall get-content "template/preamble.html"))
        (lsz-site-head (funcall get-content "template/head.html")))
   `(
     ("blog"
      :base-directory ,(concat lsz-site-path "article/blog/")
      :base-extension "org"
      :publishing-directory ,(concat lsz-site-pub-path "article/blog/")
      :publishing-function org-html-publish-to-html
      :recursive t
      :headline-levels 4

      :auto-sitemap t
      :sitemap-filename "sitemap-index.org"
      :sitemap-title "blog"

      :html-doctype "html5"
      :html-head ,lsz-site-head
      :html-preamble ,lsz-site-preamble
      :html-postamble ,lsz-site-postamble
    ;;   :htmlized-source t

      :with-toc t
      )
     ("wiki"
      :base-directory ,(concat lsz-site-path "article/wiki/")
      :base-extension "org"
      :publishing-directory ,(concat lsz-site-pub-path "article/wiki/")
      :publishing-function org-html-publish-to-html
      :recursive t
      :headline-levels 4

      :auto-sitemap t
      :sitemap-filename "sitemap-index.org"
      :sitemap-title "wiki"

      :html-doctype "html5"
      :html-head ,lsz-site-head
      :html-preamble ,lsz-site-preamble
      :html-postamble ,lsz-site-postamble
    ;;   :htmlized-source t

      :with-toc t
      )
     ("translation"
      :base-directory ,(concat lsz-site-path "article/translation/")
      :base-extension "org"
      :publishing-directory ,(concat lsz-site-pub-path "article/translation/")
      :publishing-function org-html-publish-to-html
      :recursive t
      :headline-levels 4

      :auto-sitemap t
      :sitemap-filename "sitemap-index.org"
      :sitemap-title "translation"

      :html-doctype "html5"
      :html-head ,lsz-site-head
      :html-preamble ,lsz-site-preamble
      :html-postamble ,lsz-site-postamble
    ;;   :htmlized-source t

      :with-toc t
      )
     ("site"
      :base-directory ,(concat lsz-site-path "article/site/")
      :base-extension "org"
      :publishing-directory ,(concat lsz-site-pub-path "article/site/")
      :publishing-function org-html-publish-to-html
      :recursive t
      :headline-levels 4

      :html-doctype "html5"
      :html-head ,lsz-site-head
      :html-preamble ,lsz-site-preamble
      :html-postamble ,lsz-site-postamble
    ;;   :htmlized-source t

      :with-toc nil
      )
     ("static"
      :base-directory ,(concat lsz-site-path "article_static/")
      ;; :base-extension "css\\|js\\|ico\\|png\\|jpg\\|gif\\|zip\\|7z\\|rar\\|pdf"
      :base-extension ".*"
      :publishing-directory ,(concat lsz-site-pub-path "/article_static")
      :publishing-function org-publish-attachment
      :recursive t
      )
     ("all" :components ("blog" "wiki" "site" "translation" "static"))
     )))

(provide 'org-publish-project-desc)

;;; org-publish-project-desc.el ends here
