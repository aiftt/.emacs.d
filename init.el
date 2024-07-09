;;; init.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Here be dragons

;;; Code:

;; 提高垃圾回收阀值500MB

(setq gc-cons-threshold (* 500 1024 1024))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://radian-software.github.io/straight.el/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq-default straight-vc-git-default-clone-depth 1)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;; 包管理器初始化
;; (require 'package)
;; 腾讯
;; (setq package-archives '(("gnu"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
;;                         ("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")))
;; 清华
;; (setq package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;                          ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
;;                          ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
;; emacs-china
;; (setq package-archives
;;       '(("melpa" . "http://elpa.emacs-china.org/melpa/")
;;         ("org"   . "http://elpa.emacs-china.org/org/")
;;         ("gnu"   . "http://elpa.emacs-china.org/gnu/")))
;; 科大
;; (setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ;; ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ;; ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))

;; (package-initialize)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval when
           (fboundp #'tangle-if-init)
           (add-hook 'after-save-hook #'tangle-if-init)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
