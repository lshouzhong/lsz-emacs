;; -*- coding: utf-8; -*-
;;; Require:

;;; Code:
;; --- 绑定扩展名到特定的模式
(defun add-to-alist (alist-var elt-cons &optional no-replace)
  "Add to the value of ALIST-VAR an element ELT-CONS if it isn't there yet.
If an element with the same car as the car of ELT-CONS is already present,
replace it with ELT-CONS unless NO-REPLACE is non-nil; if a matching
element is not already present, add ELT-CONS to the front of the alist.
The test for presence of the car of ELT-CONS is done with `equal'."
  (let ((existing-element (assoc (car elt-cons) (symbol-value alist-var))))
    (if existing-element
        (or no-replace
            (rplacd existing-element (cdr elt-cons)))
      (set alist-var (cons elt-cons (symbol-value alist-var)))))
  (symbol-value alist-var))

(dolist (elt-cons '(
                    ("\\.org\\'" . org-mode)
                    ("\\.stumpwmrc\\'" . lisp-mode)
                    ("\\.jl\\'" . lisp-mode)
                    ("\\.asdf\\'" . lisp-mode)
                    ("\\.clj\\'" . clojure-mode)
                    ("\\.exs\\'" . elixir-mode)
                    ("\\.[hg]s\\'" . haskell-mode)
                    ("\\.hi\\'" . haskell-mode)
                    ("\\.hs-boot\\'" . haskell-mode)
                    ("\\.chs\\'" . haskell-mode)
                    ("\\.l[hg]s\\'" . literate-haskell-mode)

                    ("\\.markdown" . markdown-mode)
                    ("\\.md" . markdown-mode)

                    ("\\.inc\\'" . asm-mode)
                    ("\\.cron\\(tab\\)?\\'" . crontab-mode)
                    ("cron\\(tab\\)?\\." . crontab-mode)
                    ("\\.a90\\'" . intel-hex-mode)
                    ("\\.hex\\'" . intel-hex-mode)

                    ("\\.py\\'" . python-mode)
                    ("SConstruct". python-mode)

                    ("\\.lua\\'" . lua-mode)

                    ("\\.go\\'" . go-mode)

                    ("\\.css\\'" . css-mode)
                    ("\\.wxss\\'" . css-mode)

                    ("\\.js.erb\\'" . js2-mode)
                    ("\\.js\\'" . js2-mode)
                    ("\\.wxs\\'" . js2-mode)

                    ("\\.ts\\'" . typescript-mode)
                    ("\\.tsx\\'" . typescript-mode)

                    ("\\.json\\'" . json-mode)
                    ("\\.ya?ml\\'" . yaml-mode)

                    ("\\.pdf\\'" . pdf-view-mode)

                    ("\\.vue" . web-mode)
                    ("\\.wxml" . web-mode)
                    ("\\.blade\\.php\\'" . web-mode)
                    ("\\.phtml\\'" . web-mode)
                    ("\\.tpl\\.php\\'" . web-mode)
                    ("\\.jsp\\'" . web-mode)
                    ("\\.as[cp]x\\'" . web-mode)
                    ("\\.erb\\'" . web-mode)
                    ("\\.mustache\\'" . web-mode)
                    ("\\.djhtml\\'" . web-mode)
                    ("\\.html?\\'" . web-mode)
                    ("\\.jsx\\'" . web-mode)

                    ("\\.iced\\'" . coffee-mode)
                    ("Cakefile\\'" . coffee-mode)
                    ("\\.coffee\\'" . coffee-mode)
                    ("\\.coffee\\'" . coffee-mode)
                    ("\\.iced\\'" . coffee-mode)
                    ("Cakefile" . coffee-mode)
                    ("\\.coffee.erb\\'" . coffee-mode)

                    ;; ("\\.rs$" . rust-mode)
                    ("CMakeLists\\.txt\\'" . cmake-mode)
                    ("\\.cmake\\'" . cmake-mode)
                    ("\\.ll\\'" . llvm-mode)
                    ))
  (add-to-alist 'auto-mode-alist elt-cons))

(add-to-list 'interpreter-mode-alist '("coffee" . coffee-mode))


;;; Mode load.
(autoload 'cmake-mode "cmake-mode")
(autoload 'css-mode "css-mode")
(autoload 'go-mode "init-golang")
(autoload 'json-mode "init-json")
(autoload 'js2-mode "js2-mode")
(autoload 'lua-mode "init-lua")
(autoload 'markdown-mode "init-markdown-mode")
(autoload 'python-mode "init-python")
(autoload 'typescript-mode "typescript-mode")
(autoload 'web-mode "web-mode")
(autoload 'yaml-mode "yaml-mode")

;; (autoload 'qml-mode "qml-mode")
;; (autoload 'coffee-mode "coffee-mode")
;; (autoload 'rust-mode "rust-mode")
;; (autoload 'qmake-mode "qmake-mode")
;; (autoload 'haskell-mode "init-haskell")
;; (autoload 'llvm-mode "llvm-mode")
;; (autoload 'elixir-mode "elixir-mode")
;; (autoload 'clojure-mode "clojure-mode")

;;; ### Auto-fill ###
;;; --- 自动换行
(setq default-fill-column 100)          ;默认显示 100列就换行
(dolist (hook (list
               'after-text-mode-hook
               'message-mode-hook
               ))
  (add-hook hook #'(lambda () (auto-fill-mode 1))))

(provide 'init-mode)

;;; init-mode.el ends here
