;;; -*- lexical-binding: t; -*-

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :init (setq rust-format-on-save t)
  :config
  (setq rust-cargo-bin "~/.cargo/bin/cargo"))

;; --- rust-mode
(provide 'init-rust)
