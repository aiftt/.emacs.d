;;; -*- lexical-binding: t; -*-

;; (require 'rime)

;; ;;; Code:
;; (setq rime-user-data-dir "~/github/mine/dot-emacs-rime/")
;; (setq rime-share-data-dir "~/Library/Rime/")
;; (setq rime-posframe-properties
;; 	    (list
;;        :internal-border-color "#000000"
;;        :internal-border-width 10
;;        ;; :font "TsangerJinKai03-6763 W05"
;;        :font "WenQuanYi Micro Hei Mono-16"
;;        ))

;; ;; “”
;; ;; 默认的前景色和背景色（仅posframe）
;; ;; 编码提示颜色 - rime-comment-face
;; ;; 编码的颜色   - rime-code-face
;; ;; 候选序号颜色 - rime-candidate-num-face
;; (set-face-attribute 'rime-default-face nil :foreground "#eeeeee" :background "#000000")
;; (setq default-input-method "rime"
;; 	    rime-show-candidate 'posframe

;; 	    ;; rime-default-face "Red" ;默认的前景色和背景色（仅posframe）
;; 	    ;; rime-code-face ;编码的颜色
;; 	    ;; rime-candidate-num-face ;候选序号颜色
;; 	    ;; rime-comment-face ;编码提示颜色

;; 	    ;; posframe/popup/sidewindow 候选版式
;; 	    ;; simple - 单行, horizontal - 水平，默认, vertical - 垂直
;; 	    rime-posframe-style 'vertical
;; 	    ;; rime-popup-style
;; 	    ;; rime-sidewindow-style
;; 	    )
;; ;; 代码中自动使用英文

;; (setq rime-disable-predicates
;; 	    '(rime-predicate-evil-mode-p ; evil 模式
;;         ;; 在英文字符串之后（必须为以字母开头的英文字符串）
;;         rime-predicate-after-alphabet-char-p
;;         ;; 在 prog-mode 和 conf-mode 中除了注释和引号内字符串之外的区域
;;         rime-predicate-prog-in-code-p
;;         ;; 激活 ace-window-mode
;;         rime-predicate-ace-window-p
;;         ;; 如果激活了一个 hydra keymap
;;         rime-predicate-hydra-p
;;         ;; 将要输入的为大写字母时
;;         rime-predicate-current-uppercase-letter-p
;;         ;; 在 (La)TeX 数学环境中或者输入 (La)TeX 命令时
;;         rime-predicate-tex-math-or-command-p
;;         ))

;; ;;;  support shift-l, shift-r, control-l, control-r
;; (setq rime-inline-ascii-trigger 'shift-l)
;; (setq rime-translate-keybindings
;; 	    '("C-`" "S-<delete>" "C-f" "C-b" "C-n" "C-p" "C-g" "<return>" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>"))
;; ;; 临时强制转成英文模式，通过 rime-inline-ascii 恢复中文模式（什么鬼～～～）
;; ;;(define-key rime-active-mode-map (kbd "M-j") 'rime-inline-ascii)
;; ;; (define-key rime-mode-map (kbd "M-j") 'rime-force-enable)
;; (define-key rime-mode-map (kbd "C-`") 'rime-send-keybinding)

;; (global-set-key (kbd "M-j") #'rime-inline-ascii)

;; (setq rime-emacs-module-header-root "/opt/homebrew/Cellar/emacs-plus@29/29.0.50/include")
;; (setq-default rime-librime-root (expand-file-name "librime/dist" user-emacs-directory))

;; (use-package sis
;;   :ensure t
;;   :init
;;   ;; `C-s/r' 默认优先使用英文 必须在 sis-global-respect-mode 前配置
;;   (setq sis-respect-go-english-triggers
;;         (list 'isearch-forward 'isearch-backward) ; isearch-forward 命令时默认进入
;;         sis-respect-restore-triggers
;;         (list 'isearch-exit 'isearch-abort))   ; isearch-forward 恢复, isearch-exit `<Enter>', isearch-abor `C-g'
;;   (sis-global-cursor-color-mode t)
;;   ;; enable the /respect/ mode buffer 输入法状态记忆模式
;;   (sis-global-respect-mode t)
;;   ;; enable the /follow context/ mode for all buffers
;;   (sis-global-context-mode t)
;;   ;; enable the /inline english/ mode for all buffers
;;   (sis-global-inline-mode t)  ; 中文输入法状态下，中文后<spc>自动切换英文，结束后自动切回中文
;;   ;; (global-set-key (kbd "M-<spc>") 'sis-switch) ; 切换输入法
;;   ;; 特殊定制
;;   (setq sis-do-set
;;         (lambda(source) (start-process "set-input-source" nil "macism" source "30000")))

;;   (setq sis-prefix-override-keys (list "C-c" "C-x" "C-h" "C-c e"))

;;   (add-hook 'org-capture-mode-hook #'sis-set-other)
;;   (setq sis-default-cursor-color "#02C389" ; 英文光标色
;;         sis-other-cursor-color "#F95B5B" ; 中文光标色
;;         sis-inline-tighten-head-rule 'zero ; 删除头部空格，默认1，删除一个空格，1/0/'all
;;         sis-inline-tighten-tail-rule 'zero ; 删除尾部空格，默认1，删除一个空格，1/0/'all
;;         sis-inline-with-english t ; 默认是t, 中文context下输入<spc>进入内联英文
;;         sis-inline-with-other t) ; 默认是nil，而且prog-mode不建议开启, 英文context下输入<spc><spc>进行内联中文
;;   :config
;;   (sis-ism-lazyman-config nil "rime" 'native))

(provide 'init-rime)
