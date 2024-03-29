;;; -*- lexical-binding: t; -*-

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
;; 自动更新 buffer
(global-auto-revert-mode)
;; 提示框的默认回答设置为 "yes"
(fset 'yes-or-no-p 'y-or-n-p)


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
 mark-ring-max 2048
 ;; 设置执行表达式的长度没有限制
 eval-expression-print-length nil
 ;; 性能优化
 gc-cons-threshold most-positive-fixnum
 ;; 设置执行表达式的长度没有限制
 eval-expression-print-length nil
 ;; 设置执行表达式的深度没有限制
 eval-expression-print-level nil
 ;; 设置最大的全局标记容量
 global-mark-ring-max 1024
 ;; minibuffer 递归调用命令
 enable-recursive-minibuffers t
 ;; 删除minibuffer的重复历史
 history-delete-duplicates t
 ;; 显示消息超时的时间
 minibuffer-message-timeout 1
 ;; 括号匹配显示但不是烦人的跳到另一个括号
 show-paren-style 'parentheses
 ;; 当插入右括号时显示匹配的左括号
 blink-matching-paren t
 ;; 不自动添加换行符到末尾, 有些情况会出现错误
 require-final-newline nil
 ;; 比较窗口设置在同一个 frame 里
 ediff-window-setup-function (quote ediff-setup-windows-plain)
 ;; 设置传送文件默认的方法
 tramp-default-method "ssh"
 ;; 禁止显示鼠标指针
 void-text-area-pointer nil
 ;; 当出现异常时弹出三角警告
 visible-bell t
 ;; 显示行尾空格
 show-trailing-whitespace t
 create-lockfiles nil
 ;; 关闭启动消息
 inhibit-startup-screen t
 inhibit-startup-message t
 inhibit-startup-echo-area-message t
 initial-scratch-message nil
 ;; 改变 *scratch* buffer 的模式
 initial-major-mode 'emacs-lisp-mode
 initial-buffer-choice t

 ;; 不要弹窗提示
 auto-revert-verbose nil
 ;; 禁用对话框
 use-dialog-box nil

 ;; 不创建备份文件。同时，也会关闭创建目录时的确认窗口
 ;; make-backup-files nil
 ;; 生成备份文件，但是这些文件备份到 /tmp 目录下
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 ;; 控制是否通过复制来创建备份文件。如果设置为非 nil 值，Emacs 将通过
 ;; 复制原始文件来创建备份文件。如果设置为 nil，则备份文件将通过重写
 ;; （link）原始文件来创建
 ;; backup-by-copying nil
 ;; 如果原始文件是一个链接（link），是否通过复制来创建备份文件。如果设
 ;; 置为非 nil 值，则当原始文件是一个链接时，备份文件将通过复制来创建
 ;; backup-by-copying-when-linked nil
 ;; 如果原始文件和备份文件的修改时间或大小不匹配，是否通过复制来创建备
 ;; 份文件。如果设置为非 nil 值，则当原始文件和备份文件的修改时间或大
 ;; 小不匹配时，备份文件将通过复制来创建
 ;; backup-by-copying-when-mismatch nil
 ;; 类似于 backup-by-copying-when-mismatch，但是当原始文件和备份文件的
 ;; 权限（权限位）不匹配时，是否通过复制来创建备份文件
 ;; backup-by-copying-when-privileged-mismatch nil
 ;; 是否在权限发生变化时通过复制来创建备份文件。如果设置为非 nil 值，
 ;; 则在原始文件的权限发生变化时，备份文件将通过复制来创建。
 ;;backup-by-copying-when-privileged nil
 ;; 禁用备份文件
 backup-inhibited t

 ;; 当你使用 find-file 或 write-file 命令打开或保存文件时，
 ;; 如果指定的目录不存在，Emacs 会自动创建该目录，而不会再弹出确认窗口
 confirm-nonexistent-file-or-buffer nil
 confirm-nonexistent-file-or-new-buffer nil

 ;; *scratch* buffer 初始显示的内容
 initial-scratch-message "\
 ;; This buffer is for notes you don't want to save, and for Ruby code.
 ;; If you want to create a file, visit that file with C-x C-f,
 ;; then enter the text in that file's own buffer."
 )

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

;; 设置行号显示的样式
(custom-set-faces
  '(line-number ((t (:foreground "#888888"))))
  '(line-number-current-line ((t (:foreg:foregrround "#888888" :weight bold)))))
;; 启用 display-line-numbers-mode
(global-display-line-numbers-mode)

(provide 'init-basic)
