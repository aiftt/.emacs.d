;;; init.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Here be dragons

;;; Code:

;; 提高垃圾回收阀值500MB
(setq gc-cons-threshold (* 500 1024 1024))

;; 包管理器初始化 
(require 'package)
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/") t)
;;   (add-to-list 'package-archives '("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/") t)
  )
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))
(package-initialize)

;; Make sure Org is installed
(unless (package-installed-p 'org)
  (package-refresh-contents)
  (package-install 'org))

;; Org plus contrib needs to be loaded before any org related functionality is called
(unless (package-installed-p 'org-contrib)
  (package-refresh-contents)
  (package-install 'org-contrib))

(require 'bind-key)

;; Load config.org - my Emacs configuration
(org-babel-load-file (concat user-emacs-directory "config.org"))

;; Garbage collector - decrease threshold to 5 MB
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 5 1024 1024))))
;;; init.el ends here
