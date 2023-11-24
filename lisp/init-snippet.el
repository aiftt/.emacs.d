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
  (yas-define-snippets 'org-mode
                       '(
                         ("<s" "#+BEGIN_SRC $1\n$0\n#+END_SRC" "src")
                         ("<j" "#+BEGIN_SRC javascript $1\n$0\n#+END_SRC" "src")
                         ("<c" "#+BEGIN_SRC css $1\n$0\n#+END_SRC" "src")
                         ("<h" "#+BEGIN_SRC html $1\n$0\n#+END_SRC" "src")
                         ("<v" "#+BEGIN_SRC vue $1\n$0\n#+END_SRC" "src")
                         ("<e" "#+BEGIN_EXAMPLE $1\n$0\n#+END_EXAMPLE" "src")
                         )))

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
