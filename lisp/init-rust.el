;;; -*- lexical-binding: t; -*-

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :init (setq rust-format-on-save t))

;; --- rust-mode
(provide 'init-rust)
