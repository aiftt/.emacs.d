;; --- 函数定义
(defun max-gc-limit ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun reset-gc-limit ()
  (setq gc-cons-threshold 100000000))

;; <f5> 刷新配置
(defun gcl/reload-init-file ()
  "Reload init file with <f5>."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(defun gcl/open-init-file()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

;; --- 我的信息
(setq blog-admin-dir "~/github/mine/blog.cheng92.com/")
(setq user-full-name "Lee ZhiCheng"
      user-mail-address "ftt.loves@gmail.com"
      user-blog-url "https://blog.cheng92.com"
      user-github-dir "~/github/mine/"
      user-web-dir "~/github/mine/gcl-web-system/"
      user-blog-dir (concat user-web-dir "apps/blog/")
      user-blog-public-dir (concat user-blog-dir "public/")
      user-blog-posts (concat user-web-dir "posts/")
      user-dot-dir "~/.gclrc/"
      user-dot-bin-dir "~/.gclrc/bin/"
      )

;; --- 垃圾回收
(add-hook 'minibuffer-setup-hook #'max-gc-limit)
(add-hook 'minibuffer-exit-hook #'reset-gc-limit)
(setq-default bidi-display-reordering nil)

;; --- 体验优化
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; 选中粘贴时能覆盖选中的内容
(delete-selection-mode 1)
;; 高亮当前行
(global-hl-line-mode 1)
;; 指针不闪动。
(blink-cursor-mode -1)
;; 启动全屏
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)


;; --- 基础设置
(setq
 ;; 加速启动
 auto-mode-case-fold nil
 ;; 加快快捷键提示的速度
 process-adaptive-read-buffering nil
 ;; 提升 IO 性能
 echo-keystrokes 0.1
 ;; 增加单次读取进程输出的数据量（缺省 4KB)
 read-process-output-max (* 1024 1024)
 ;; 缩短 fontify 时间
 jit-lock-defer-time nil
 jit-lock-context-time 0.1
 ;; 更积极的 fontify
 fast-but-imprecise-scrolling nil
 redisplay-skip-fontification-on-input nil
 ;; 使用字体缓存，避免卡顿
 inhibit-compacting-font-caches t
 ;; 使用更瘦字体
 ns-use-thin-smoothing t
 ;; 缩短更新 screen 的时间
 idle-update-delay 0.1
 ;; 错误提示级别
 warning-minimum-level :emergency
 ;; 不要缩放frame.
 frame-inhibit-implied-resize t
 ;; 默认用最简单的模式
 initial-major-mode 'fundamental-mode
 ;; 不要自动启用package
 package-enable-at-startup nil
 package--init-file-ensured t
 ;; 阻止文件变更时弹窗提示
 revert-without-query '(".*")
 ;; 默认显示 80 列就换行
 default-fill-column 80
 ;; 用一个很大的 kill ring. 这样防止我不小心删掉重要的东西
 kill-ring-max 2048
 ;; 设置的 mark ring 容量
 mark-ring-max 1024
 ;; 设置执行表达式的长度没有限制
 eval-expression-print-length nil
 ;; 性能优化
 gc-cons-threshold most-positive-fixnum)


;; --- 添加 hooks
;; 有些功能需要用到，比如：折叠等等
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; --- custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(and (file-readable-p custom-file) (load custom-file))

;; --- 环境路径
;; 直接将环境变量拷贝到 ~/.path 中
;; sh -c 'printf "%s" "$PATH"' > ~/.path
(condition-case err
    (let ((path (with-temp-buffer
		  (insert-file-contents-literally "~/.path")
		  (buffer-string))))
      (setenv "PATH" path)
      (setq exec-path (append (parse-colon-path path) (list exec-directory))))
  (error (warn "%s" (error-message-string err))))

;;; https://github.com/purcell/emacs.d
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(and (file-readable-p custom-file) (load custom-file))

;;; --- package setting
(cl-defun slot/vc-install (&key (fetcher "github") repo name rev backend)
  "Install a package from a remote if it's not already installed.
This is a thin wrapper around `package-vc-install' in order to
make non-interactive usage more ergonomic.  Takes the following
named arguments:

- FETCHER the remote where to get the package (e.g., \"gitlab\").
  If omitted, this defaults to \"github\".

- REPO should be the name of the repository (e.g.,
  \"slotThe/arXiv-citation\".

- NAME, REV, and BACKEND are as in `package-vc-install' (which
  see)."
  (let* ((url (format "https://www.%s.com/%s" fetcher repo))
         (iname (when name (intern name)))
         (pac-name (or iname (intern (file-name-base repo)))))
    (unless (package-installed-p pac-name)
      (package-vc-install url iname rev backend))))

;; (use-package math-delimiters
;;  :init (slot/vc-install :fetcher "github" :repo "oantolin/math-delimiters")
;;  ;; OR (slot/vc-install :repo "oantolin/math-delimiters")
;;  )

;; --- 环境变量设置
(setenv "NODE_PATH" "/usr/local/lib/node_modules")

;; purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :init (slot/vc-install :repo "purcell/exec-path-from-shell")
  :config
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "LSP_USE_PLISTS" "NODE_PATH")
	exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;; --- 开发设置

;; - auto-save
(use-package auto-save
  :init (slot/vc-install :repo "manateelazycat/auto-save")
  :config
  (auto-save-enable)
  (setq auto-save-silent t)
  (setq auto-save-delete-trailing-whitespace t))

;; --- 按键设置
(global-set-key (kbd "<f5>") 'gcl/reload-init-file)
(global-set-key (kbd "<f1>") 'gcl/open-init-file)
;; (global-set-key (kbd "<f2>") 'restart-emacs)

;; --- 清理
(defun gcl/cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold  (* 1024 200)) ; 200MB
  (setq gc-cons-percentage 0.5) ; 0.5s
  (garbage-collect))
(run-with-idle-timer 4 nil #'gcl/cleanup-gc)
