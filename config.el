(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; 光标样式
(setq-default cursor-type '(bar . 1))
;; 光标不闪烁
(blink-cursor-mode -1)

;; 去掉工具栏等
(when window-system
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (tooltip-mode 0))

;; 高亮当前行
(global-hl-line-mode 1)
;; 显示列号
(column-number-mode t)

;; 启动全屏
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)

;; 启动界面
(setq inhibit-startup-message t
      initial-scratch-message ""
      initial-major-mode 'emacs-lisp-mode
      initial-buffer-choice t
      inhibit-splash-screen t)

;; - 选中粘贴时能覆盖选中的内容
(delete-selection-mode 1)

;; - 文件编码
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-auto-unix)

;; - 错误信息
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; - 截断行
(setq-default truncate-lines t)
;; - yes or no
(fset 'yes-or-no-p 'y-or-n-p)
;; - 驼峰单词里移动
(subword-mode)
;; - 默认认为两个空格开头为一个段落，关闭此选项
(setq sentence-end-double-space nil)
;; - 更好的通配符搜索
(setq search-whitespace-regexp ".*?")
;; - 历史记录
(savehist-mode)
;; - 窗口管理
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; - 在 mac 上，当进入一个新的工作空间时，会默认全屏
(setq ns-use-native-fullscreen nil)

;; - 不生成备份文件
(setq make-backup-files nil)
;; 分割窗口的时候自动切换到该窗口
;; (defadvice split-window (after move-point-to-new-window activate)
;;   "Moves the point to the newly created window after splitting."
;;   (other-window 1))

(setq
 ;; 缩短更新 screen 的时间
 idle-update-delay 0.1
 ;; 加速启动
 auto-mode-case-fold nil
 ;; 加快快捷键提示的速度
 process-adaptive-read-buffering nil
 ;; 提升 IO 性能
 echo-keystrokes 0.1
 ;; 增加单次读取进程输出的数据量（缺省 4KB)
 read-process-output-max (* 1024 1024)

 ;; 性能优化
 gc-cons-threshold 100000000

 ;; 括号匹配显示但不是烦人的跳到另一个括号
 show-paren-style 'parentheses
 ;; 当插入右括号时显示匹配的左括号
 blink-matching-paren t

 ;; 不自动添加换行符到末尾, 有些情况会出现错误
 require-final-newline nil

 ;; 比较窗口设置在同一个 frame 里
 ediff-window-setup-function (quote ediff-setup-windows-plain)
 )

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(global-set-key (kbd "<f1>")
                (lambda ()
                  (interactive)
                  (find-file "~/.emacs.d/config.org")))

(global-set-key (kbd "<f5>") 'reload-init-file)

(defun reload-init-file ()
  "重新加载 init.el 文件的函数"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; 直接将环境变量拷贝到 ~/.path 中
;; sh -c 'printf "%s" "$PATH"' > ~/.path
(condition-case err
    (let ((path (with-temp-buffer
		  (insert-file-contents-literally "~/.path")
		  (buffer-string))))
      (setenv "PATH" path)
      (setq exec-path (append (parse-colon-path path) (list exec-directory))))
  (error (warn "%s" (error-message-string err))))

(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "LSP_USE_PLISTS" "NODE_PATH")
	exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(defun sk/diminish-auto-revert ()
  (interactive)
  (diminish 'auto-revert-mode ""))
(add-hook 'auto-revert-mode-hook 'sk/diminish-auto-revert)

(use-package async :commands (async-start))
(use-package cl-lib)
(use-package dash)
(use-package s)
(use-package hydra)

(use-package symbol-overlay
  :defer t
  :config
  (symbol-overlay-mode +1)
  (global-set-key (kbd "M-i") #'symbol-overlay-put)
  (global-set-key (kbd "M-n") #'symbol-overlay-switch-forward)
  (global-set-key (kbd "M-p") #'symbol-overlay-switch-backward)
  (global-set-key (kbd "<f7>") #'symbol-overlay-mode)
  (global-set-key (kbd "<f8>") #'symbol-overlay-remove-all)
  )

(use-package maple-iedit
  :straight (:type git :host github :repo "honmaple/emacs-maple-iedit")
  :commands (maple-iedit-match-all maple-iedit-match-next maple-iedit-match-previous)
  :config
  (setq maple-iedit-ignore-case t)

  (defhydra maple/iedit ()
    ("n" maple-iedit-match-next "next")
    ("t" maple-iedit-skip-and-match-next "skip and next")
    ("T" maple-iedit-skip-and-match-previous "skip and previous")
    ("p" maple-iedit-match-previous "prev"))
  :bind* (("C-," . maple/iedit/body)))

(use-package expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(use-package diminish
  :demand t
  :diminish (visual-line-mode . "ω")
  :diminish hs-minor-mode
  :diminish abbrev-mode
  :diminish auto-fill-function
  :diminish subword-mode
  :diminish eldoc-mode
  :diminish which-key-mode
  :diminish persp-mode
  )

(use-package restart-emacs
  :bind* (("<f2>" . restart-emacs)))

(use-package auto-save
  :straight (auto-save :type git :host github :repo "manateelazycat/auto-save")
  :config
  (auto-save-enable)
  (setq auto-save-silent t)
  (setq auto-save-delete-trailing-whitespace t))

(defvar gcl/default-font-size 150)
(defvar gcl/default-variable-font-size 150)

;; Make frame transparency overridable
(defvar gcl/frame-transparency '(90 . 90))

(defvar gcl/variable-pitch-size 120)
(defvar gcl/org-heading-font "Iosevka Aile"
  "The font used for Org Mode headings.")

  (set-face-attribute 'default nil :font "Fira Code Retina" :height gcl/default-font-size)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height gcl/default-font-size)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font "Cantarell" :height gcl/default-variable-font-size :weight 'regular)

(use-package doom-themes
   :config
   ;; Global settings (defaults)
   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
         doom-themes-enable-italic t) ; if nil, italics is universally disabled
   (load-theme 'doom-one t)

   ;; Enable flashing mode-line on errors
   (doom-themes-visual-bell-config)
   ;; Enable custom neotree theme (all-the-icons must be installed!)
   (doom-themes-neotree-config)
   ;; or for treemacs users
   (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
   (doom-themes-treemacs-config)
   ;; Corrects (and improves) org-mode's native fontification.
   (doom-themes-org-config))

(use-package doom-modeline
  :init (progn
          (setq doom-modeline-env-version nil
                doom-modeline-icon nil
                doom-modeline-minor-modes t)
          (doom-modeline-mode 1)))

(use-package org-tempo
  :straight (:type built-in)
  :after org
  :config
  (dolist (item '(("sh" . "src sh")
                  ("el" . "src emacs-lisp")
                  ("li" . "src lisp")
                  ("sc" . "src scheme")
                  ("ts" . "src typescript")
                  ("py" . "src python")
                  ("html" . "src html")
                  ("vue" . "src vue")
                  ("go" . "src go")
                  ("einit" . "src emacs-lisp :tangle ~/.config/emacs/init.el :mkdirp yes")
                  ("emodule" . "src emacs-lisp :tangle ~/.config/emacs/modules/dw-MODULE.el :mkdirp yes")
                  ("yaml" . "src yaml")
                  ("json" . "src json")))
    (add-to-list 'org-structure-template-alist item)))

(defun tangle-if-init ()
  "If the current buffer is 'config.org' the code-blocks are
    tangled, and the tangled file is compiled."
  (when (string-suffix-p "config.org" (buffer-file-name))
    (tangle-init)))

(defun tangle-init-sync ()
  (interactive)
  (message "Tangling init")
  ;; Avoid running hooks when tangling.
  (let ((prog-mode-hook nil)
  (src  (expand-file-name "config.org" user-emacs-directory))
  (dest (expand-file-name "config.el"  user-emacs-directory)))
    (require 'ob-tangle)
    (org-babel-tangle-file src dest)
    (if (byte-compile-file dest)
  (byte-compile-dest-file dest)
(with-current-buffer byte-compile-log-buffer
  (buffer-string)))))

(defun tangle-init ()
  "Tangle init.org asynchronously."
  (interactive)
  (message "Tangling init")
  (async-start
    (symbol-function #'tangle-init-sync)
    (lambda (result)
      (message "Init tangling completed: %s" result))))
