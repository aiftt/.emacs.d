;;; -*- lexical-binding: t; -*-

;; - snippets
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1)
  (yas-reload-all)
  )

;; (use-package yasnippet-snippets
;;  :ensure t
;;  :defer t
;;  :after yasnippet)

;; (with-eval-after-load 'yasnippet
;;  (require 'yasnippet-snippets)
;;  (yas-global-mode 1)
;;  (add-hook 'prog-mode-hook 'yas-minor-mode)
;;   (add-hook 'org-mode-hook 'yas-minor-mode)
;;  )

(provide 'init-snippet)
