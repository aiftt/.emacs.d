;;; -*- lexical-binding: t; -*-

;; - auto-save
(use-package auto-save
  :init (slot/vc-install :repo "manateelazycat/auto-save")
  :config
  (auto-save-enable)
  (setq auto-save-silent t)
  (setq auto-save-delete-trailing-whitespace t))


;; --- tools
(use-package restclient :ensure t)
(use-package httprepl :ensure t)
(use-package nvm :ensure t :defer t)
(use-package crux :ensure t)
(use-package uuidgen :ensure t)
(use-package dash-at-point :ensure t)

(use-package link-hint
  :ensure t
  :bind
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link))

;; Test: Result 6.23+(3.789/(5-4)) + 6.4*(2 - (5+3) *736.83 ) /2000
(use-package math-at-point
  :init (slot/vc-install :repo "shankar2k/math-at-point"))


(provide 'init-tools)
