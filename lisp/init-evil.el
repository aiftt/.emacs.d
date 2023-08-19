;;; -*- lexical-binding: t; -*-

;; --- evil
(use-package evil
  :ensure t
  :init
  (evil-mode)
  :config
  ;; 退出编辑模式后光标留在原地
  (setq evil-move-cursor-back nil)
  ;; 让回车，TAB，空格键保持原来的功能
  (with-eval-after-load 'evil-maps
    ;; (define-key evil-motion-state-map (kbd "RET") nil)
    (define-key evil-motion-state-map (kbd "TAB") nil)
    (define-key evil-motion-state-map (kbd "SPC") nil))

  (progn
    ;; --- 解绑一些按键
    (evil-global-set-key 'normal (kbd "c") nil)

    (define-key evil-motion-state-map (kbd "E") 'evil-end-of-line)
    (define-key evil-motion-state-map (kbd "0") 'evil-end-of-line)

    (evil-global-set-key 'normal "f" 'evil-avy-goto-char)
    (evil-global-set-key 'normal "w" 'evil-avy-goto-word-or-subword-1)
    (evil-global-set-key 'normal "s" 'evil-avy-goto-line)

    ;; (evil-global-set-key 'motion "-" 'org-decrease-number-at-point)
    ;; (evil-global-set-key 'motion "+" 'org-increase-number-at-point)

    (evil-global-set-key 'normal (kbd "gm") 'magit)
    (evil-global-set-key 'normal (kbd "gc") 'show-commit-and-preserve-window)

    (evil-global-set-key 'normal (kbd "cc") 'evilnc-copy-and-comment-lines)
    ))

;; 设置一些 buffer 中 evil 状态
(evil-set-initial-state 'multi-vterm-mode 'emacs)
(evil-set-initial-state 'vterm-mode 'emacs)
(evil-set-initial-state 'magit-mode 'emacs)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'magit-branch-manager-mode 'emacs)
(evil-set-initial-state 'color-rg-mode 'emacs)

(use-package evil-nerd-commenter
  :ensure t)

(use-package evil-surround
  :ensure t
  :config
  (setq-default evil-surround-pairs-alist
		            '((?\( . ("(" . ")"))
                  (?\[ . ("[" . "]"))
                  (?\{ . ("{" . "}"))

                  (?\) . ("( " . " )"))
                  (?\] . ("[ " . " ]"))
                  (?\} . ("{ " . " }"))

                  (?# . ("#{" . "}"))
                  (?b . ("(" . ")"))
                  (?B . ("{" . "}"))
                  (?> . ("<" . ">"))

                  (?\/ . ("/* " . " */"))

                  ;; Single-quoted strings
                  (?\' . ("'" . "'"))

                  ;; Emacs-style quotes
                  (?\` . ("`" . "'"))
		              ;; javascript
                  (?\` . ("`" . "`"))

                  ;; Python multi-line strings
                  (?d . ("\"\"\"" . "\"\"\""))
                  (?D . ("'''" . "'''"))

                  (?t . evil-surround-read-tag)
                  (?< . evil-surround-read-tag)
                  (?f . evil-surround-function)))
  )
(global-evil-surround-mode 1)

(use-package ace-pinyin
  :ensure t
  :custom
  (ace-pinyin-global-mode +1)
  (ace-pinyin-treat-word-as-char nil))

(provide 'init-evil)
