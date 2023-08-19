;;; -*- lexical-binding: t; -*-

;; --- 环境变量设置
(setenv "NODE_PATH" "/usr/local/lib/node_modules")

;; purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "LSP_USE_PLISTS" "NODE_PATH")
 	      exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(provide 'init-env)
