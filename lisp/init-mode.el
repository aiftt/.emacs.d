;;; -*- lexical-binding: t; -*-

;; - modes
;; 绑定扩展名到特定的模式
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
                    ("\\.prettierrc" . yaml-mode)
                    ("\\.markdown" . markdown-mode)
                    ("\\.md" . markdown-mode)
                    ("\\.stumpwmrc\\'" . lisp-mode)
                    ("\\.org\\'" . org-mode)
                    ("\\.cron\\(tab\\)?\\'" . crontab-mode)
                    ("cron\\(tab\\)?\\." . crontab-mode)
                    ("\\.py$" . python-mode)
                    ("SConstruct". python-mode)
                    ("\\.jl\\'" . lisp-mode)
                    ("\\.asdf\\'" . lisp-mode)
                    ("CMakeLists\\.txt\\'" . cmake-mode)
                    ("\\.cmake\\'" . cmake-mode)
                    ("\\.php\\'" . php-mode)
                    ("\\.wxml" . web-mode)
                    ("\\.blade\\.php\\'" . web-mode)
                    ("\\.phtml\\'" . web-mode)
                    ("\\.tpl\\.php\\'" . web-mode)
                    ("\\.jsp\\'" . web-mode)
                    ("\\.as[cp]x\\'" . web-mode)
                    ("\\.erb\\'" . web-mode)
                    ("\\.mustache\\'" . web-mode)
                    ("\\.djhtml\\'" . web-mode)
                    ("\\.js.erb\\'" . js-mode)
                    ("\\.css\\'" . css-mode)
                    ("\\.wxss\\'" . css-mode)
                    ("\\.go$" . go-mode)
                    ("\\.rs$" . rust-mode)
		                ;; ----------------------------->
                    ("\\.html?\\'" . web-mode)
                    ("\\.vue" . web-mode)
                    ("\\.wxs$" . js-mode)
                    ("\\.cjs$" . js-mode)
                    ("\\.mjs$" . js-mode)
                    ("\\.js$" . js-mode)
                    ("\\.jsx$" . web-mode)
                    ("\\.tsx$" . web-mode)
                    ("\\.ts$" . typescript-mode)
                    ("_gcl$" . sh-mode)
                    ("\\.env\\.*" . sh-mode)
                    ("\\.schema$" . prisma-mode)
		                ;; <-----------------------------
                    ("\\.lua$" . lua-mode)
                    ("\\.pdf$" . pdf-view-mode)
                    ("\\.cpp$" . c++-mode)
                    ("\\.h$" . c++-mode)
                    ("\\.ll$" . llvm-mode)
                    ("\\.bc$" . hexl-mode)
                    ("\\.json$" . json-mode)
                    ("\\.svg$" . xml-mode)
		                ("\\.http$" . restclient-mode)
		                ("\\.rs$" . rust-mode)
		                ("\\Dockerfile$" . dockerfile-mode)
                    ))
  (add-to-alist 'auto-mode-alist elt-cons))

(add-to-list 'interpreter-mode-alist '("coffee" . coffee-mode))
(add-hook 'web-mode-hook 'emmet2-mode)


;; 自动换行
(dolist (hook (list
               'after-text-mode-hook
               'message-mode-hook
               ))
  (add-hook hook #'(lambda () (auto-fill-mode 1))))


(provide 'init-mode)
