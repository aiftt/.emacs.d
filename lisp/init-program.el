;;; -*- lexical-binding: t; -*-

;; --- yaml
(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'"
  :mode "\\.yaml\\'"
  :hook ((yaml-mode . yaml-imenu-enable)))
(use-package yaml-imenu
  :ensure t
  :after yaml-mode)
(use-package python-mode :ensure t)
(use-package go-mode :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package php-mode :ensure t)
(use-package sql-indent :ensure t)
(add-hook 'sql-mode-hook 'sqlind-minor-mode)
(use-package pkg-info :ensure t)
(use-package lua-mode :ensure t)
(use-package format-all :ensure t)
(add-hook 'before-save-hook 'format-all-buffer nil t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; --- orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


(provide 'init-program)
