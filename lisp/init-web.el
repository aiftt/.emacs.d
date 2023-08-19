;;; -*- lexical-binding: t; -*-

;; --- web
(defun my/setup-js-mode ()
  (setq js-indent-level 2)
  (setq evil-shift-width js-indent-level)
  (setq tab-width 2))

(use-package js2-mode
  :ensure t
  :mode "\\.jsx?\\'"
  :config
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)

  ;; Set up proper indentation in JavaScript and JSON files
  (add-hook 'js2-mode-hook #'my/setup-js-mode)
  (add-hook 'json-mode-hook #'my/setup-js-mode))

(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'")
  :hook ((typescript-mode . my/setup-js-mode)))

(use-package json-mode :ensure t)
(use-package css-mode :ensure t)
(use-package scss-mode :ensure t)
(use-package emmet-mode
  :ensure t
  :hook ((sgml-mode html-mode css-mode web-mode typescript-mode js-mode) . emmet-mode)
  :config
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))))


(use-package web-mode
  :ensure t
  :mode
  (
   ".twig$"
   ".html?$"
   ".hbs$"
   ".vue$"
   ".blade.php$"
   )
  :config
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-style-padding 0
   web-mode-script-padding 0
   web-mode-enable-auto-closing t
   web-mode-enable-auto-opening t
   web-mode-enable-auto-pairing nil
   web-mode-enable-auto-indentation t
   web-mode-tag-auto-close-style 1
   web-mode-enable-current-element-highlight t)

  ;; 设置不同类型代码的注释格式
  (setq web-mode-comment-formats
        '(("javascript" . "//")    ; JavaScript 注释
          ("jsx" . "//")           ; JSX 注释
          ("php" . "//")           ; PHP 注释
          ("css" . "/*")           ; CSS 注释
          ("java" . "//")          ; Java 注释
          ;; 添加更多类型的注释格式
          ))

  ;; Let smartparens handle auto closing brackets, e.g. {{ }} or {% %}
  ;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/web/%2Bhtml.el#L56
  (dolist (alist web-mode-engines-auto-pairs)
    (setcdr alist
	          (cl-loop for pair in (cdr alist)
		                 unless (string-match-p "^[a-z-]" (cdr pair))
		                 collect (cons (car pair)
				                           (string-trim-right (cdr pair)
							                                        "\\(?:>\\|]\\|}\\)+\\'")))))
  ;; (add-to-list 'lsp-language-id-configuration '(web-mode . "vue"))
  )

(use-package js-doc
  :ensure t
  :config
  (setq js-doc-mail-address user-mail-address
        js-doc-author (format "<%s> <%s>" user-full-name js-doc-mail-address)
        js-doc-url user-blog-url
        js-doc-license "MIT"))

;; --- development 开发设置

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; tab4
(setq c-basic-offset 4
      sh-basic-offset 4
      sh-indentation 4
      coffee-tab-width 4
      )

;; tab2
(setq javascript-indent-level 2
      js-indent-level 2
      js2-basic-offset 2
      typescript-indent-offset 2
      typescript-indent-level 2
      css-indent-offset 2
      web-mode-attr-indent-offset 2
      web-mode-attr-value-indent-offset 2
      web-mode-code-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-markup-indent-offset 2
      web-mode-sql-indent-offset 2)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(use-package dumb-jump
  :ensure t)

(provide 'init-web)
