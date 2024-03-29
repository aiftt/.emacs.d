;;; -*- lexical-binding: t; -*-

;; - lsp-bridge
(require 'lsp-bridge)
(require 'lsp-bridge-jdtls)
(require 'acm-backend-tailwind)
(setq lsp-bridge-python-command "/usr/bin/python3")
(global-lsp-bridge-mode)
(setq acm-enable-tabnine nil)
;; 根据文件扩展名设置 lsp server
(setq lsp-bridge-single-lang-server-extension-list '((("wxml")
                                                      . "wxml-language-server")
                                                     (("html")
                                                      . "vscode-html-language-server")
                                                     (("tsx") . "typescriptreact")
                                                     ))

(setq lsp-bridge-default-mode-hooks
      '(
        ;; java-mode-hook
        typescript-tsx-mode-hook
        typescript-mode-hook
        js2-mode-hook
        js-mode-hook
        rjsx-mode-hook
        web-mode-hook
        ;; org-mode-hook
        lisp-interaction-mode-hook

        )
      )

;; 融合 `lsp-bridge' `find-function' 以及 `dumb-jump' 的智能跳转
(defun lsp-bridge-jump ()
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (let ((symb (function-called-at-point)))
      (when symb
        (find-function symb))))
   (lsp-bridge-mode
    (lsp-bridge-find-def))
   (t
    (require 'dumb-jump)
    (dumb-jump-go))))

(defun lsp-bridge-jump-back ()
  (interactive)
  (cond
   (lsp-bridge-mode
    (lsp-bridge-find-def-return))
   (t
    (require 'dumb-jump)
    (dumb-jump-back))))

;; --- deno
(setq lsp-bridge-get-single-lang-server-by-project
      (lambda (project-path filepath)
        ;; If typescript first line include deno.land, then use Deno LSP server.
        (save-excursion
          (when (string-equal (file-name-extension filepath) "ts")
            (dolist (buf (buffer-list))
              (when (string-equal (buffer-file-name buf) filepath)
		            (with-current-buffer buf
                  (goto-char (point-min))
                  (when (string-match-p (regexp-quote "from \"https://deno.land") (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                    (return "deno")))))))))

;; 打开日志，开发者才需要
(setq lsp-bridge-enable-log nil)

(setq acm-backend-lsp-candidate-min-length 2)
(setq acm-backend-elisp-candidate-min-length 2)
(setq acm-backend-yas-candidate-min-length 1)
(setq acm-backend-codeium-candidate-min-length 2)

(with-eval-after-load 'evil
  (evil-global-set-key 'normal (kbd "gd") 'lsp-bridge-jump)
  (evil-global-set-key 'normal (kbd "gb") 'lsp-bridge-jump-back)
  (evil-global-set-key 'normal (kbd "gf") 'lsp-bridge-find-references)
  (evil-set-initial-state 'lsp-bridge-ref-mode 'emacs)
  )

(provide 'init-lsp)
