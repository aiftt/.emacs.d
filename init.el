;; --- 函数定义
(defun add-subdirs-to-load-path (search-dir)
  (interactive)
  (let* ((dir (file-name-as-directory search-dir)))
    (dolist (subdir
             ;; 过滤出不必要的目录，提升Emacs启动速度
             (cl-remove-if
              #'(lambda (subdir)
                  (or
                   ;; 不是目录的文件都移除
                   (not (file-directory-p (concat dir subdir)))
                   ;; 父目录、 语言相关和版本控制目录都移除
                   (member subdir '("." ".."
                                    "dist" "node_modules" "__pycache__"
                                    "RCS" "CVS" "rcs" "cvs" ".git" ".github"))))
              (directory-files dir)))
      (let ((subdir-path (concat dir (file-name-as-directory subdir))))
        ;; 目录下有 .el .so .dll 文件的路径才添加到 `load-path' 中，提升Emacs启动速度
        (when (cl-some #'(lambda (subdir-file)
                           (and (file-regular-p (concat subdir-path subdir-file))
                                ;; .so .dll 文件指非Elisp语言编写的Emacs动态库
                                (member (file-name-extension subdir-file) '("el" "so" "dll"))))
                       (directory-files subdir-path))

          ;; 注意：`add-to-list' 函数的第三个参数必须为 t ，表示加到列表末尾
          ;; 这样Emacs会从父目录到子目录的顺序搜索Elisp插件，顺序反过来会导致Emacs无法正常启动
          (add-to-list 'load-path subdir-path t))

        ;; 继续递归搜索子目录
        (add-subdirs-to-load-path subdir-path)))))

(defun max-gc-limit ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun reset-gc-limit ()
  (setq gc-cons-threshold 100000000))

;; --- system
(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

;; time date
(defun gcl/insert-standard-date ()
  "Inserts standard date time string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %T")))

(defun gcl/insert-changelog-date ()
  "Insert changelog date, like yyyy/mm/dd."
  (interactive)
  (insert (format-time-string "%Y/%m/%d")))

(defun gcl/insert-current-time ()
  "Insert current time, like hh:mm:ss."
  (interactive)
  (insert (format-time-string "%T")))

(defun gcl/open-current-directory ()
  (interactive)
  (consult-file-externally default-directory))

(defun gcl/copy-file-full-name ()
  "Copy the current buffer's file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun gcl/copy-file-name-only ()
  "Copy the current buffer's file name only to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new (file-name-nondirectory filename))
      (message "Copied buffer file name '%s' to the clipboard." (file-name-nondirectory filename)))))

(defun gcl/kill-current-buffer ()
  (interactive)
  (kill-current-buffer)
  (switch-to-prev-buffer))

(defun buf-move-up ()
  "Swap the current buffer and the buffer above the split.
If there is no split, ie now window above the current one, an
error is signaled."
  ;;  "Switches between the current buffer, and the buffer above the
  ;;  split, if possible."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'up))
	       (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No window above this one")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

(defun buf-move-down ()
  "Swap the current buffer and the buffer under the split.
If there is no split, ie now window under the current one, an
error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'down))
	       (buf-this-buf (window-buffer (selected-window))))
    (if (or (null other-win)
            (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win))))
        (error "No window under this one")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

(defun buf-move-left ()
  "Swap the current buffer and the buffer on the left of the split.
If there is no split, ie now window on the left of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'left))
	       (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No left split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

(defun buf-move-right ()
  "Swap the current buffer and the buffer on the right of the split.
If there is no split, ie now window on the right of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'right))
	       (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No right split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))


(defun kill-other-window-buffer ()
  "Kill the buffer in other window."
  (interactive)
  (other-window +1)
  (kill-this-buffer)
  (other-window -1))

(defun gcl/get-frame->selected-window ()
  "Returns a list of pairs of (frame selected-window)"
  (let* ((original-frame (window-frame))
         (result (->> (visible-frame-list)
                      (-map (lambda (f)
                              (select-frame f t)
                              (list f (selected-window)))))))
    (select-frame original-frame t)
    result))

(eval-when-compile
  (require 'cl))
(defun gcl/preserve-selected-window (f)
  "Runs the given function and then restores focus to the original window. Useful when you want to invoke
   a function (like showing documentation) but desire to keep your current window focused."
  ;; Note that we must preserve the selected window of every frame, because the function being executed may
  ;; change the focused frame, even if the current frame is in focus.
  (lexical-let* ((original-frame (selected-frame))
                 (frames->windows (gcl/get-frame->selected-window))
                 (result (funcall f)))
    (-each frames->windows (lambda (x)
                             (select-frame (first x) t)
                             (select-window (second x) t)))
    (select-frame-set-input-focus original-frame t)
    result))


(defun split-window--select-window (orig-func &rest args)
  "Switch to the other window after a `split-window'"
  (let ((cur-window (selected-window))
        (new-window (apply orig-func args)))
    (when (equal (window-buffer cur-window) (window-buffer new-window))
      (select-window new-window))
    new-window))
(advice-add 'split-window :around #'split-window--select-window)

(defun gcl/cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold  (* 1024 200)) ; 200MB
  (setq gc-cons-percentage 0.5) ; 0.5s
  (garbage-collect))

(defun switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer (startup--get-buffer-create-scratch)))

;; <f5> 刷新配置
(defun gcl/reload-init-file ()
  "Reload init file with <f5>."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(defun gcl/open-init-file()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

;; -- load path
(add-subdirs-to-load-path "~/.emacs.d/extensions/")

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

;; --- 包源配置
(require 'package)
(setq package-archives '(("gnu"   . "http://1.15.88.122/gnu/")
			 ("melpa" . "http://1.15.88.122/melpa/")))
(package-initialize)

;; --- 环境变量设置
(setenv "NODE_PATH" "/usr/local/lib/node_modules")

;; purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "LSP_USE_PLISTS" "NODE_PATH")
 	exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

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
;; (evil-set-initial-state 'multi-vterm-mode 'emacs)
;; (evil-set-initial-state 'vterm-mode 'emacs)
(evil-set-initial-state 'magit-mode 'emacs)
(evil-set-initial-state 'dired-mode 'emacs)
;; (evil-set-initial-state 'magit-branch-manager-mode 'emacs)
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

;; --- ui 界面管理
(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :ensure t
  :hook ((dired-mode . all-the-icons-dired-mode)))
(use-package posframe
  :ensure t)

(use-package highlight-parentheses
  :ensure t
  :hook (prog-mode . highlight-parentheses-mode)
  :config
  (add-hook 'minibuffer-setup-hook #'highlight-parentheses-minibuffer-setup)
  )

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :defer t
  :hook ((prog-mode org-mode) . rainbow-mode))

;; - header line
(set-face-attribute 'header-line nil
                    :foreground (face-attribute 'mode-line :foreground)
                    :background (face-attribute 'mode-line :background)
                    ;; height of mode-line is also unspecified, so we set it directly.
                    :height 150
                    :box (face-attribute 'mode-line :box))

;; - cursor background color
(set-face-attribute 'cursor nil :background "green")
(set-face-attribute 'default nil :font "Fira Code" :height 130)

;; - theme
(use-package doom-themes
  :ensure t
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

;; Must be used *after* the theme is loaded
(custom-set-faces
  ;; `(mode-line ((t (:background ,(doom-color 'dark-violet)))))
  `(font-lock-comment-face ((t (:foreground ,(doom-color 'base6))))))

;; - mode line
(use-package awesome-tray
  :init (slot/vc-install :repo "manateelazycat/awesome-tray")
  :config
  (setq awesome-tray-mode-line-height 0.1
	awesome-tray-mode-line-active-color "#EC4899"
	awesome-tray-mode-line-inactive-color "#959eb1"
	awesome-tray-active-modules '(
				    ;; "location"
				    "pdf-view-page"
				    "date"
				    "file-path"
				    "buffer-name"
				    "mode-name"
				    "battery"
				    "git"
				    "input-method"
				    "evil"
				    ;; "flymake"
				    "belong"
				    "anzu"
				    ;; "github"
				    )
	awesome-tray-date-format "%d/%H:%M:%S")
  (awesome-tray-mode 1))

;; --- buffer 管理

(use-package marginalia
  :ensure t
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package all-the-icons-completion
  :ensure t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package vertico
  :ensure t
  :config
  (setq vertico-count 13
      vertico-resize t
      vertico-cycle nil)
  (vertico-mode)
  (require 'vertico-repeat)
  (add-hook 'minibuffer-setup-hook 'vertico-repeat-save)
  )

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; --- embark
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; - consult
(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  ;; (setq consult-async-min-input 2)

  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  )

(use-package saveplace
  :ensure t
  :hook (after-init . save-place-mode))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
	 :map minibuffer-local-completion-map
	 ("C-x C-d" . consult-dir)
	 ("C-x C-j" . consult-dir-jump-file))
  )


(use-package consult-ls-git
  :ensure t
  :bind
  (("C-c g f" . #'consult-ls-git)
   ("C-c g F" . #'consult-ls-git-other-window)))

(use-package consult-notes
  :ensure t
  :commands (consult-notes
             consult-notes-search-in-all-notes
             ;; if using org-roam
             consult-notes-org-roam-find-node
             consult-notes-org-roam-find-node-relation)
  :bind
  (("C-c n /" . consult-notes-search-in-all-notes)
   ("C-c n <" . consult-notes-org-roam-find-node)
   ("C-c n >" . consult-notes-org-roam-find-node-relation)
   )
  :config
  (setq consult-notes-sources
        '(("Posts"             ?p "~/.posts")
          ("Org"      ?r "~/.gclrc/org")))
  ;; Set org-roam integration OR denote integration
  (when (locate-library "denote")
    (consult-notes-denote-mode)))

(use-package consult-org-roam
  :ensure t
  :after org-roam
  :init
  (require 'consult-org-roam)
  ;; Activate the minor mode
  (consult-org-roam-mode 1)
  :custom
  ;; Use `ripgrep' for searching with `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t)
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-."))
  )


(use-package consult-projectile
  :ensure t)

(use-package consult-yasnippet
  :ensure t)

(use-package consult-project-extra
  :ensure t
  :bind
  (("C-c p ," . consult-project-extra-find)
   ("C-c p ." . consult-project-extra-find-other-window)))


;; --- fanyi
;; --- fanyi
(use-package fanyi
  :ensure t
  :config
  (custom-set-variables
   '(fanyi-providers '(fanyi-haici-provider
			                 fanyi-youdao-thesaurus-provider
			                 fanyi-etymon-provider
			                 fanyi-longman-provider
			                 ;; fanyi-libre-provider
			                 )))
  ;; 还要自动选择翻译内容 buffer
  (setq fanyi-auto-select nil))


(use-package youdao-dictionary
  :ensure t)

(use-package sdcv
  :ensure t
  :config
  ;; say word after translation
  (setq sdcv-say-word-p t)
  ;; setup directory of stardict dictionary
  (setq sdcv-dictionary-data-dir (concat user-emacs-directory "sdcv-dict"))
  (setq sdcv-dictionary-simple-list    ;setup dictionary list for simple search
      '("懒虫简明英汉词典"
        "懒虫简明汉英词典"
        "KDic11万英汉词典"))
  (setq sdcv-dictionary-complete-list     ;setup dictionary list for complete search
      '(
        "懒虫简明英汉词典"
        "英汉汉英专业词典"
        "XDICT英汉辞典"
        "stardict1.3英汉辞典"
        "WordNet"
        "XDICT汉英辞典"
        "Jargon"
        "懒虫简明汉英词典"
        "FOLDOC"
        "新世纪英汉科技大词典"
        "KDic11万英汉词典"
        "朗道汉英字典5.0"
        "CDICT5英汉辞典"
        "新世纪汉英科技大词典"
        "牛津英汉双解美化版"
        "21世纪双语科技词典"
        "quick_eng-zh_CN"
        ))
  )

;; --- magit
(use-package magit
  :ensure t
  :config
  ;; 提交时候不显示提交细节
  (setq magit-commit-show-diff nil)
  ;; 没有焦点时候不刷新状态
  (setq magit-refresh-status-buffer nil)
  ;; 当前buffer打开magit
  (setq magit-display-buffer-function
	(lambda (buffer)
          (display-buffer buffer '(display-buffer-same-window))))
  (setq magit-ellipsis (get-byte 0 "."))
  ;; 加速diff
  (setq magit-revision-insert-related-refs nil)
  (setq magit-diff-refine-hunk t)
  (setq magit-diff-paint-whitespace nil)
  (setq magit-ediff-dwim-show-on-hunks t)
  (setq magit-display-buffer-function
	(lambda (buffer)
	  (display-buffer buffer '(display-buffer-same-window))))
  ;; 加速diff
  (setq magit-revision-insert-related-refs nil)
  )

(defun show-commit-and-preserve-window ()
    (interactive)
    ;; NOTE(philc): I'm not sure why magit-show-commit needs to be called interactively, but just invoking it
    ;; directly gives an argument error.
    (gcl/preserve-selected-window (lambda ()
                                    (call-interactively 'magit-show-commit))))

(use-package blamer
  :ensure t
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  (blamer-author-formatter " ✎ %s ")
  (blamer-datetime-formatter "[%s]")
  (blamer-commit-formatter " ● %s")
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    ;; :background nil
                    :height 120
                    :italic t)))
  :config
  ;; (global-blamer-mode 1)
  )

(use-package git-modes
  :ensure t
  :config
  (add-to-list 'auto-mode-alist
	       (cons "/.dockerignore\\'" 'gitignore-mode))
  (add-to-list 'auto-mode-alist
	       (cons "/.gitignore\\'" 'gitignore-mode))
  (add-to-list 'auto-mode-alist
               (cons "/.gitconfig\\'" 'gitconfig-mode))
  )

(use-package smerge-mode)

;; --- persp
(use-package perspective
  :ensure t
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c TAB"))  ; pick your own prefix key here
  :init
  (persp-mode)
  :config
  (setq persp-state-default-file (expand-file-name ".cache/gcl" user-emacs-directory))
  (setq persp-show-modestring 'header)
  (add-hook 'kill-emacs-hook #'persp-state-save)
  (use-package persp-projectile
    :ensure t)
  )

(with-eval-after-load 'general
  (general-define-key
   "s-1" '(lambda () (interactive) (persp-switch-by-number 1))
   "s-2" '(lambda () (interactive) (persp-switch-by-number 2))
   "s-3" '(lambda () (interactive) (persp-switch-by-number 3))
   "s-4" '(lambda () (interactive) (persp-switch-by-number 4))
   "s-5" '(lambda () (interactive) (persp-switch-by-number 5))
   "s-)" 'persp-next
   "s-(" 'persp-prev
   ))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;; alien, hybrid
  (setq projectile-indexing-method 'alien projectile-enable-caching t)
  )

;; --- web
(defun my/setup-js-mode ()
  (setq js-indent-level 2)
  (setq evil-shift-width js-indent-level)
  (setq tab-width 2))

(use-package js2-mode
  :ensure t
  :mode "\\.jsx?\\'"
  :config
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)

  ;; Set up proper indentation in JavaScript and JSON files
  (add-hook 'js2-mode-hook #'my/setup-js-mode)
  (add-hook 'json-mode-hook #'my/setup-js-mode))

(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'")
  :hook ((typescript-mode . my/setup-js-mode)))

(use-package json-mode :ensure t)
(use-package css-mode :ensure t)
(use-package scss-mode :ensure t)
(use-package emmet-mode
  :ensure t
  :hook ((sgml-mode html-mode css-mode web-mode typescript-mode js-mode) . emmet-mode)
  :config
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))))


(use-package web-mode
  :ensure t
  :mode
  (
   ".twig$"
   ".html?$"
   ".hbs$"
   ".vue$"
   ".blade.php$"
   )
  :config
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-style-padding 0
   web-mode-script-padding 0
   web-mode-enable-auto-closing t
   web-mode-enable-auto-opening t
   web-mode-enable-auto-pairing nil
   web-mode-enable-auto-indentation t
   web-mode-tag-auto-close-style 1
   web-mode-enable-current-element-highlight t)

  ;; Let smartparens handle auto closing brackets, e.g. {{ }} or {% %}
  ;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/web/%2Bhtml.el#L56
  (dolist (alist web-mode-engines-auto-pairs)
    (setcdr alist
	          (cl-loop for pair in (cdr alist)
		                 unless (string-match-p "^[a-z-]" (cdr pair))
		                 collect (cons (car pair)
				                           (string-trim-right (cdr pair)
							                                        "\\(?:>\\|]\\|}\\)+\\'")))))
  ;; (add-to-list 'lsp-language-id-configuration '(web-mode . "vue"))
  )

(use-package js-doc
  :ensure t
  :config
  (setq js-doc-mail-address user-mail-address
       js-doc-author (format "<%s> <%s>" user-full-name js-doc-mail-address)
       js-doc-url user-blog-url
       js-doc-license "MIT"))

;; --- development 开发设置

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; tab4
(setq c-basic-offset 4
      sh-basic-offset 4
      sh-indentation 4
      coffee-tab-width 4
      )

;; tab2
(setq javascript-indent-level 2
      js-indent-level 2
      js2-basic-offset 2
      typescript-indent-offset 2
      typescript-indent-level 2
      css-indent-offset 2
      web-mode-attr-indent-offset 2
      web-mode-attr-value-indent-offset 2
      web-mode-code-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-markup-indent-offset 2
      web-mode-sql-indent-offset 2)

;; - modes
;; 绑定扩展名到特定的模式
(defun add-to-alist (alist-var elt-cons &optional no-replace)
  "Add to the value of ALIST-VAR an element ELT-CONS if it isn't there yet.
If an element with the same car as the car of ELT-CONS is already present,
replace it with ELT-CONS unless NO-REPLACE is non-nil; if a matching
element is not already present, add ELT-CONS to the front of the alist.
The test for presence of the car of ELT-CONS is done with `equal'."
  (let ((existing-element (assoc (car elt-cons) (symbol-value alist-var))))
    (if existing-element
        (or no-replace
            (rplacd existing-element (cdr elt-cons)))
      (set alist-var (cons elt-cons (symbol-value alist-var)))))
  (symbol-value alist-var))

(dolist (elt-cons '(
                    ("\\.markdown" . markdown-mode)
                    ("\\.md" . markdown-mode)
                    ("\\.stumpwmrc\\'" . lisp-mode)
                    ("\\.org\\'" . org-mode)
                    ("\\.cron\\(tab\\)?\\'" . crontab-mode)
                    ("cron\\(tab\\)?\\." . crontab-mode)
                    ("\\.py$" . python-mode)
                    ("SConstruct". python-mode)
                    ("\\.jl\\'" . lisp-mode)
                    ("\\.asdf\\'" . lisp-mode)
                    ("CMakeLists\\.txt\\'" . cmake-mode)
                    ("\\.cmake\\'" . cmake-mode)
                    ("\\.php\\'" . php-mode)
                    ("\\.wxml" . web-mode)
                    ("\\.blade\\.php\\'" . web-mode)
                    ("\\.phtml\\'" . web-mode)
                    ("\\.tpl\\.php\\'" . web-mode)
                    ("\\.jsp\\'" . web-mode)
                    ("\\.as[cp]x\\'" . web-mode)
                    ("\\.erb\\'" . web-mode)
                    ("\\.mustache\\'" . web-mode)
                    ("\\.djhtml\\'" . web-mode)
                    ("\\.js.erb\\'" . js-mode)
                    ("\\.css\\'" . css-mode)
                    ("\\.wxss\\'" . css-mode)
                    ("\\.go$" . go-mode)
                    ("\\.rs$" . rust-mode)
		                ;; ----------------------------->
                    ("\\.html?\\'" . web-mode)
                    ("\\.vue" . web-mode)
                    ("\\.wxs$" . js-mode)
                    ("\\.cjs$" . js-mode)
                    ("\\.mjs$" . js-mode)
                    ("\\.js$" . js-mode)
                    ("\\.jsx$" . web-mode)
                    ("\\.tsx$" . web-mode)
                    ("\\.ts$" . typescript-mode)
                    ("_gcl$" . sh-mode)
                    ("\\.env\\.*" . sh-mode)
                    ("\\.schema$" . prisma-mode)
		                ;; <-----------------------------
                    ("\\.lua$" . lua-mode)
                    ("\\.pdf$" . pdf-view-mode)
                    ("\\.cpp$" . c++-mode)
                    ("\\.h$" . c++-mode)
                    ("\\.ll$" . llvm-mode)
                    ("\\.bc$" . hexl-mode)
                    ("\\.json$" . json-mode)
                    ("\\.svg$" . xml-mode)
		                ("\\.http$" . restclient-mode)
		                ("\\.rs$" . rust-mode)
		                ("\\Dockerfile$" . dockerfile-mode)
                    ))
  (add-to-alist 'auto-mode-alist elt-cons))

(add-to-list 'interpreter-mode-alist '("coffee" . coffee-mode))

;; 自动换行
(dolist (hook (list
               'after-text-mode-hook
               'message-mode-hook
               ))
  (add-hook hook #'(lambda () (auto-fill-mode 1))))

;; --- yaml
(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'"
  :mode "\\.yaml\\'"
  :hook ((yaml-mode . yaml-imenu-enable)))
(use-package yaml-imenu
  :ensure t
  :after yaml-mode)
(use-package python-mode :ensure t)
(use-package go-mode :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package php-mode :ensure t)
(use-package sql-indent :ensure t)
(add-hook 'sql-mode-hook 'sqlind-minor-mode)
(use-package pkg-info :ensure t)
(use-package lua-mode :ensure t)
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :init (setq rust-format-on-save t))

(use-package format-all :ensure t)
(add-hook 'before-save-hook 'format-all-buffer nil t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; --- orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; --- edit
(use-package expand-region
  :ensure t)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'duplicate-line)

(use-package autorevert
  :diminish
  :hook (after-init . global-auto-revert-mode))
(use-package hungry-delete
  :ensure t
  :diminish
  :config
  (setq-default hungry-delete-chars-to-skip " \t\f\v"))
(global-hungry-delete-mode)

;; --- hydra
(use-package hydra :ensure t)
;; --- smerge
(defhydra hydra-smerge (:color red :hint nil)
  "
Navigate       Keep               other
----------------------------------------
_p_: previous  _c_: current       _e_: ediff
_n_: next      _m_: mine  <<      _u_: undo
_j_: up        _o_: other >>      _r_: refine
_k_: down      _a_: combine       _q_: quit
               _b_: base
"
  ("n" smerge-next)
  ("p" smerge-prev)
  ("c" smerge-keep-current)
  ("m" smerge-keep-mine)
  ("o" smerge-keep-other)
  ("b" smerge-keep-base)
  ("a" smerge-keep-all)
  ("e" smerge-ediff)
  ("j" previous-line)
  ("k" forward-line)
  ("r" smerge-refine)
  ("u" undo)
  ("q" nil :exit t))

(defhydra hydra-color-rg (:color green :hint nil)
  "
搜索当前目录          搜索当前项目           搜索当前文件
------------------------------------------------------
_i_: Input          _a_: Input           _x_: Input
_s_: Symbol         _b_: Symbol          _y_: Symbol
_t_: Type           _c_: Type

"
  ;; current dir
  ("i" color-rg-search-input)
  ("s" color-rg-search-symbol)
  ("t" color-rg-search-symbol-with-type)
  ;; current project
  ("a" color-rg-search-input-in-project)
  ("b" color-rg-search-symbol-in-project)
  ("c" color-rg-search-project-with-type)
  ;; current file
  ("x" color-rg-search-input-in-current-file)
  ("y" color-rg-search-symbol-in-current-file)
  ("q" nil :exit t))

(defhydra hydra-everything (:hint nil :columns 3 :exit t)
  "快捷操作
-------------------------------------------------------------------------------------"
  ("1" gcl/open-init-file "打开Emacs配置")
  )


(use-package symbol-overlay :ensure t)
(use-package move-text :ensure t)
(use-package iedit :ensure t)
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))
(use-package toggle-quotes-plus
  :init (slot/vc-install :repo "jcs-elpa/toggle-quotes-plus")
  :config
  (setq toggle-quotes-plus-chars '("\""
                                 "'"
                                 "`")))
(global-set-key (kbd "C-'") #'toggle-quotes-plus)

(use-package maple-iedit
  :init (slot/vc-install :repo "honmaple/emacs-maple-iedit")
  :commands (maple-iedit-match-all maple-iedit-match-next maple-iedit-match-previous)
  :config
  (setq maple-iedit-ignore-case t)

  (defhydra maple/iedit ()
    ("n" maple-iedit-match-next "next")
    ("t" maple-iedit-skip-and-match-next "skip and next")
    ("T" maple-iedit-skip-and-match-previous "skip and previous")
    ("p" maple-iedit-match-previous "prev"))
  :bind (:map evil-visual-state-map
              ("n" . maple/iedit/body)
              ("C-n" . maple-iedit-match-next)
              ("C-p" . maple-iedit-match-previous)
              ("C-t" . maple-iedit-skip-and-match-next)))

(use-package string-inflection
  :ensure t)

(defun my-string-inflection-cycle-auto ()
    "switching by major-mode"
    (interactive)
    (cond
     ;; for emacs-lisp-mode
     ((eq major-mode 'emacs-lisp-mode)
      (string-inflection-all-cycle))
     ;; for python
     ((eq major-mode 'python-mode)
      (string-inflection-python-style-cycle))
     ;; for java
     ((eq major-mode 'java-mode)
      (string-inflection-java-style-cycle))
     ;; for elixir
     ((eq major-mode 'elixir-mode)
      (string-inflection-elixir-style-cycle))
     (t
      ;; default
      (string-inflection-ruby-style-cycle))))

(use-package parrot
  :ensure t
  :config
  (parrot-mode -1)
(setq parrot-rotate-dict
      '(
        (:rot ("alpha" "beta") :caps t :lower nil)
        ;; => rotations are "Alpha" "Beta"

        (:rot ("snek" "snake" "stawp"))
        ;; => rotations are "snek" "snake" "stawp"

        (:rot ("yes" "no") :caps t :upcase t)
        ;; => rotations are "yes" "no", "Yes" "No", "YES" "NO"

        (:rot ("&" "|"))
        ;; => rotations are "&" "|"

        ;; default dictionary starts here ('v')
        (:rot ("begin" "end") :caps t :upcase t)
        (:rot ("enable" "disable") :caps t :upcase t)
        (:rot ("enter" "exit") :caps t :upcase t)
        (:rot ("forward" "backward") :caps t :upcase t)
        (:rot ("front" "rear" "back") :caps t :upcase t)
        (:rot ("get" "set") :caps t :upcase t)
        (:rot ("high" "low") :caps t :upcase t)
        (:rot ("in" "out") :caps t :upcase t)
        (:rot ("left" "right") :caps t :upcase t)
        (:rot ("min" "max") :caps t :upcase t)
        (:rot ("on" "off") :caps t :upcase t)
        (:rot ("prev" "next"))
        (:rot ("start" "stop") :caps t :upcase t)
        (:rot ("true" "false") :caps t :upcase t)
        (:rot ("&&" "||"))
        (:rot ("==" "!="))
        (:rot ("." "->"))
        (:rot ("if" "else" "elif"))
        (:rot ("ifdef" "ifndef"))
        (:rot ("int8_t" "int16_t" "int32_t" "int64_t"))
        (:rot ("uint8_t" "uint16_t" "uint32_t" "uint64_t"))
        (:rot ("1" "2" "3" "4" "5" "6" "7" "8" "9" "10"))
        (:rot ("1st" "2nd" "3rd" "4th" "5th" "6th" "7th" "8th" "9th" "10th"))

        ;; mine
        (:rot ("let" "const" "var"))
        )))

(use-package separedit
  :ensure t)

(defun separedit//region-of-swagger-commentary ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "* @swagger\n")
      (let ((begin (point)))
        (when (re-search-forward  "\s+\\*/" nil t)
          (goto-char (match-beginning 0))
          (list begin (point)))))))

(defun separedit/edit-swagger-commentary ()
  "Edit whole commentary section as a single block."
  (interactive)
  (let ((separedit-leave-blank-line-in-comment t))
    (separedit-dwim
     (apply #'separedit-mark-region
            `(,@(separedit//region-of-swagger-commentary)
              yaml-mode)))))

;; - snippets
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1)
  (yas-reload-all)
  )

(use-package yasnippet-snippets
  :ensure t
  :defer t
  :after yasnippet)

(with-eval-after-load 'yasnippet
  (require 'yasnippet-snippets)
  (yas-global-mode 1)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'org-mode-hook 'yas-minor-mode)
  )

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(use-package dumb-jump
  :ensure t)

;; - lsp-bridge
(require 'lsp-bridge)
(require 'lsp-bridge-jdtls)
(require 'acm-backend-tailwind)
(global-lsp-bridge-mode)
(setq acm-enable-tabnine nil)
;; 根据文件扩展名设置 lsp server
(setq lsp-bridge-single-lang-server-extension-list '((("wxml")
                                                      . "wxml-language-server")
                                                     (("html")
                                                      . "vscode-html-language-server")
                                                     (("tsx") . "typescriptreact")
                                                     ))

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

(with-eval-after-load 'evil
  (evil-global-set-key 'normal (kbd "gd") 'lsp-bridge-jump)
  (evil-global-set-key 'normal (kbd "gb") 'lsp-bridge-jump-back)
  (evil-global-set-key 'normal (kbd "gf") 'lsp-bridge-find-references)
  (evil-set-initial-state 'lsp-bridge-ref-mode 'emacs)
  )

;; - auto-save
(use-package auto-save
  :init (slot/vc-install :repo "manateelazycat/auto-save")
  :config
  (auto-save-enable)
  (setq auto-save-silent t)
  (setq auto-save-delete-trailing-whitespace t))

;; --- search
(use-package devdocs
  :ensure t)

(use-package engine-mode
  :ensure t
  :config
  (engine-mode t)
  (engine/set-keymap-prefix (kbd "C-c s"))
  (defengine baidu "https://www.baidu.com/s?wd=%s"
	     :keybinding "b")
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")
  (defengine qwant
    "https://www.qwant.com/?q=%s"
    :docstring "什么都能搜到哦~~😍😍"
    :keybinding "q")
  (defengine rfcs
    "http://pretty-rfc.herokuapp.com/search?q=%s"
    :keybinding "r")
  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")
  (defengine twitter
    "https://twitter.com/search?q=%s"
    :keybinding "t")
  (defengine wolfram-alpha
    "http://www.wolframalpha.com/input/?i=%s"
    :docstring "数学搜索引擎，公式，坐标图等。"
    :keybinding "w")
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "/")
  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y")
  )

(use-package wgrep
  :ensure t)
(setq wgrep-auto-save-buffer t)

(use-package rg
  :ensure t)

(use-package visual-regexp
  :ensure t)

(use-package visual-regexp-steroids
  :ensure t)

(use-package color-rg
  :init (slot/vc-install :repo "manateelazycat/color-rg"))

;; --- org
(with-eval-after-load 'org
  (progn
    (setq org-directory "~/.gclrc/org/"
          org-startup-indented t
          org-pretty-entities t
          org-hide-emphasis-markers t
          org-startup-with-inline-images t
          org-image-actual-width '(300)
          org-html-doctype "html5")
    )

  ;; -- 使用 “+” 来切换列表风格，- -> 1. -> a. ...
  (evil-define-key 'normal org-mode-map
    "+" #'org-cycle-list-bullet)

  ;; -- keywords
  (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                  (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)" "MEETING(m)" "PHONE(p)"))))

  ;; -- paint
  (setq org-plantuml-jar-path "~/.gclrc/plantuml.jar")
  (setq org-ditaa-jar-path "~/.gclrc/ditaa.jar")

  ;; -- emphasis
  (setq org-emphasis-alist
          '(("*" my-org-emphasis-bold)
            ("/" my-org-emphasis-italic)
            ("_" underline)
            ("=" org-verbatim verbatim)
            ("~" org-code verbatim)
            ("+" (:strike-through t))))
  (defface my-org-emphasis-bold
      '((default :inherit bold)
        (((class color) (min-colors 88) (background light))
         :foreground "#a60000")
        (((class color) (min-colors 88) (background dark))
         :foreground "#ff8059"))
      "My bold emphasis for Org.")

  (defface my-org-emphasis-italic
      '((default :inherit italic)
        (((class color) (min-colors 55) (background light))
         :foreground "#972500")
        (((class color) (min-colors 55) (background dark))
         :foreground "#ef8b50"))
      "My italic emphasis for Org.")

  ;; Allow multiple line Org emphasis markup.
  ;; http://emacs.stackexchange.com/a/13828/115
  (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just 1
  ;; Below is needed to apply the modified `org-emphasis-regexp-components'
  ;; settings from above.
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  )

(use-package org-super-agenda
  :ensure t
  :config
  (defvar org-agenda-dir ""
    "gtd org files location")

  (defvar deft-dir ""
    "deft org files locaiton")

  (setq org-agenda-dir "~/.gclrc/org/")
  (setq deft-dir  "~/.gclrc/org/")
  (setq org-agenda-log-mode-items '(clock closed state))

  (setq org-agenda-inhibit-startup t) ;; ~50x speedup
  (setq org-agenda-span 'day)
  (setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
  (setq org-agenda-window-setup 'current-window)
  (setq org-log-done t)
  (setq org-columns-default-format "%60ITEM(Task) %6Effort(Estim){:}")

  (setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
  (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
  (setq org-agenda-file-work (expand-file-name "work.org" org-agenda-dir))
  (setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
  (setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
  (setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
  (setq org-agenda-file-blogposts (expand-file-name "all-posts.org" org-agenda-dir))
  (setq org-agenda-files (list org-agenda-file-gtd org-agenda-file-journal org-agenda-file-blogposts org-agenda-file-work org-agenda-file-note))
  )


(use-package svg-tag-mode
  :ensure t
  :after org
  :hook (org-mode . svg-tag-mode)
  :config
  (defun mk/svg-checkbox-empty ()
    (let* ((svg (svg-create 14 14)))
      (svg-rectangle svg 0 0 14 14 :fill 'white :rx 2 :stroke-width 2.5 :stroke-color 'black)
      (svg-image svg :ascent 'center)))

  (defun mk/svg-checkbox-filled ()
    (let* ((svg (svg-create 14 14)))
      (svg-rectangle svg 0 0 14 14 :fill "#FFFFFF" :rx 2)
      (svg-polygon svg '((5.5 . 11) (12 . 3.5) (11 . 2) (5.5 . 9) (1.5 . 5) (1 . 6.5))
                   :stroke-color 'black :stroke-width 1 :fill 'black)
      (svg-image svg :ascent 'center)))

  (defun mk/svg-checkbox-toggle ()
    (interactive)
    (save-excursion
      (let* ((start-pos (line-beginning-position))
             (end-pos (line-end-position))
             (text (buffer-substring-no-properties start-pos end-pos))
             (case-fold-search t)       ; Let X and x be the same in search
             )
        (beginning-of-line)
        (cond ((string-match-p "\\[X\\]" text)
               (progn
                 (re-search-forward "\\[X\\]" end-pos)
                 (replace-match "[ ]")))
              ((string-match-p "\\[ \\]" text)
               (progn
                 (search-forward "[ ]" end-pos)
                 (replace-match "[X]")))))))

  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                      nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag (concat value "%")
                             nil :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
           (count (float (car seq)))
           (total (float (cadr seq))))
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar (/ count total) nil
                                        :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                  (svg-lib-tag value nil
                               :stroke 0 :margin 0)) :ascent 'center)))

  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

  (setq svg-tag-tags
        `(
          ;; -- Number
          ("\([0-9a-zA-Z]\)" . ((lambda (tag)
                                  (svg-tag-make tag :beg 1 :end -1 :radius 12))))
          ;; -- Task priority
          ("\\[#[A-Z]\\]" . ((lambda (tag)
                               (svg-tag-make tag :face 'org-priority
                                             :beg 2 :end -1 :margin 0))))
          ;; -- Tags
          ("\\(:#[A-Za-z0-9]+\\)" . ((lambda (tag)
                                       (svg-tag-make tag :beg 2))))
          ("\\(:#[A-Za-z0-9]+:\\)$" . ((lambda (tag)
                                         (svg-tag-make tag :beg 2 :end -1))))

          ;; -- Progress
          ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                              (svg-progress-percent (substring tag 1 -2)))))
          ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                            (svg-progress-count (substring tag 1 -1)))))

          ;; -- Checkbox
          ("\\[ \\]" . ((lambda (_tag) (mk/svg-checkbox-empty))
                        (lambda () (interactive) (mk/svg-checkbox-toggle))
                        "Click to toggle."))
          ("\\(\\[[Xx]\\]\\)" . ((lambda (_tag) (mk/svg-checkbox-filled))
                                 (lambda () (interactive) (mk/svg-checkbox-toggle))
                                 "Click to toggle."))

          ;; -- Date: Active date (with or without day name, with or without time)
          (,(format "\\(<%s>\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0))))
          (,(format "\\(<%s \\)%s>" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
          (,(format "<%s \\(%s>\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

          ;; -- Date: Inactive date  (with or without day name, with or without time)
          (,(format "\\(\\[%s\\]\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
          (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
          (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))

          ;; Keywords
          ;; ("TODO" . ((lambda (tag) (svg-tag-make tag :height 0.8 :inverse t
          ;;                                        :face 'org-todo :margin 0 :radius 5))))
          ;; ("WORK" . ((lambda (tag) (svg-tag-make tag :height 0.8
          ;;                                        :face 'org-todo :margin 0 :radius 5))))
          ;; ("DONE" . ((lambda (tag) (svg-tag-make tag :height 0.8 :inverse t
          ;;                                        :face 'org-done :margin 0 :radius 5))))

          ("FIXME\\b" . ((lambda (tag) (svg-tag-make "FIXME" :face 'org-todo :inverse t :margin 0 :crop-right t))))
          ("DONE\\b" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :inverse t :margin 0 :crop-right t))))
          ("TODO\\b" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-tag :inverse t :margin 0 :crop-right t))))
          ("WORK\\b" . ((lambda (tag) (svg-tag-make "WORK" :face 'org-todo :inverse t :margin 0 :crop-right t))))
          ))

  (require 'org-tempo)
  (require 'org-src)
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation t
        ;; or current-window
        org-src-window-setup 'other-window)
   (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
   (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
   (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
   (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
   (add-to-list 'org-structure-template-alist '("py" . "src python"))
   (add-to-list 'org-structure-template-alist '("go" . "src go"))
   (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
   (add-to-list 'org-structure-template-alist '("json" . "src json"))

   (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (python . t)
      (shell . t)
      (js . t)))

   ;; 设置 JavaScript 代码块的默认语言为 "js"
   (setq org-babel-default-header-args:js
         '((:session . "none")
           (:results . "replace")
           (:exports . "both")
           (:cache . "no")
           (:noweb . "no")
           (:hlines . "no")
           (:tangle . "no")))
   )

;; https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org
(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () org-superstar-mode))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; appear
(use-package org-appear
  :ensure t
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (defun org-apperance-evil-hack ()
    (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
    (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t))
  (setq org-appear-trigger 'manual)
  (add-hook 'org-mode-hook #'org-apperance-evil-hack)
  )

(use-package org-superstar
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  (setq org-superstar-special-todo-items t)
  )

(use-package org-mac-link
  :ensure t)

(use-package org-fragtog
  :ensure t
  :hook (org-mode . org-fragtog-mode))

;; org-download
(use-package org-download
  :ensure t
  :hook (dired-mode . org-download-enable)
  :config
  (setq-default org-download-heading-lvl nil
                org-download-image-dir "~/.img/tmp/"
                ;; org-download-screenshot-method "screencapture -i %s"
                org-download-screenshot-method "pngpaste %s"
                org-download-screenshot-file (expand-file-name "screenshot.jpg" temporary-file-directory))
  )


;; --- org-roam
(use-package org-roam
  :ensure t
  :after org
  :custom
  (org-roam-directory (file-truename user-blog-posts))
  (org-roam-dailies-directory "daily/")
  :config
  (defun gcl/org-capture-slipbox ()
    (interactive)
    (org-capture nil "s"))
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (setq org-roam-capture-templates
        '(("m" "main" plain "%?"
           :if-new (file+head "main/${slug}.org"
                              "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("x" "reference" plain "%?"
           :if-new
           (file+head "reference/${title}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("o" "others" plain "%?"
           :if-new
           (file+head "others/${title}.org"
                      "#+title: ${title}\n#+created: %U\n#+filetags: :others:\n#+startup: overview hideblocks")
           :immediate-finish t
           :unnarrowed t)
          ("v" "vue" plain "%?"
           :if-new
           (file+head "vue/${title}.org"
                      "#+title: ${title}\n#+created: %U\n#+filetags: :vue:\n#+startup: overview hideblocks\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "react" plain "%?"
           :if-new
           (file+head "react/${title}.org"
                      "#+title: ${title}\n#+created: %U\n#+filetags: :react:\n#+startup: overview hideblocks")
           :immediate-finish t
           :unnarrowed t)
          ("w" "web" plain "%?"
           :if-new
           (file+head "web/${title}.org"
                      "#+title: ${title}\n#+created: %U\n#+filetags: :web:\n#+startup: overview hideblocks")
           :immediate-finish t
           :unnarrowed t)
          ("e" "emacs" plain "%?"
           :if-new
           (file+head "emacs/${title}.org"
                      "#+title: ${title}\n#+created: %U\n#+filetags: :emacs:\n#+startup: overview hideblocks")
           :immediate-finish t
           :unnarrowed t)
          ))

  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  )

;; org-roam-ui
(use-package org-roam-ui
  :ensure t
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil
	      org-roam-ui-browser-function #'browse-url
	      ))

;; --- org-roam bindings
(general-define-key
 "C-c n n" 'org-roam-node-find
 "C-c n g" 'org-roam-graph
 "C-c n r" 'org-roam-node-random
 "C-c n c" 'org-roam-capture
 "C-c n d" 'org-roam-dailies-capture-today
 "C-c n u" 'org-roam-ui-open
 "C-c n f" 'consult-org-roam-file-find
 "C-c n b" 'consult-org-roam-backlinks
 "C-c n l" 'consult-org-roam-forward-links
 "C-c n s" 'consult-org-roam-search
 )

(general-def org-mode-map
  "C-c n i" 'org-roam-node-insert
  "C-c n o" 'org-id-get-create
  "C-c n t" 'org-roam-tag-add
  ;; "C-c n e" 'org-roam-extract-subtree
  "C-c n a" 'org-roam-alias-add
  "C-c n ," 'org-roam-buffer-toggle)

;; org bindings
(evil-define-key 'normal org-mode-map
  "tt" 'org-todo
  "tp" 'org-priority
  "td" 'org-deadline
  "tc" 'org-capture
  "tl" 'org-store-link
  "tn" 'org-add-note
  "t," 'org-toggle-checkbox

  ;; clock
  "ci" 'org-clock-in
  "co" 'org-clock-out
  "cg" 'org-clock-goto
  "cx" 'org-clock-cancel
  "ck" 'org-clock-timestamps-up
  "cj" 'org-clock-timestamps-down
  )

;; --- window
(use-package toggle-one-window
  :init (slot/vc-install :repo "manateelazycat/toggle-one-window"))

(use-package watch-other-window)

(setq display-buffer-base-action
      '(
        ;; 当新的 buffer 与当前 window 显示的 buffer 具有相同的 major
        ;; mode 时，重用当前 window 显示该 buffer
        display-buffer-reuse-mode-window
        ;; 尝试重用当前 window 显示新的 buffer，不管 major mode 是否相同
        display-buffer-reuse-window
        ;; 尝试在当前 window 显示新的 buffer，如果没有其他窗口可用
        display-buffer-same-window)
      ;; 默认情况下，Emacs 会尽量保持窗口大小均匀分布，但这会导致某些
      ;; 情况下窗口大小不符合期望，因此通过设置 even-window-sizes 为
      ;; nil 可以禁用这个自动平衡功能
      even-window-sizes nil)


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

;; --- vterm
(use-package vterm :ensure t)
(use-package multi-vterm :ensure t)
(use-package vterm-toggle :ensure t)
(add-hook 'vterm-mode-hook
          (lambda ()
            (setq-local evil-insert-state-cursor 'box)
            (evil-insert-state)))
(with-eval-after-load 'vterm
  (define-key vterm-mode-map [return] #'vterm-send-return)
  (define-key vterm-mode-map [(control return)]   #'vterm-toggle-insert-cd)
  (define-key vterm-mode-map (kbd "s-n")   'vterm-toggle-forward)
  (define-key vterm-mode-map (kbd "s-p")   'vterm-toggle-backward)
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 ;;(display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 ;;(direction . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3))))


;; --- which-key
(use-package which-key
  :hook (after-init . which-key-mode)
  :ensure t
  :init
  (setq which-key-side-window-location 'bottom)
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 0.1)
  ;;(setq which-key-idle-secondary-delay 0.05)
  (which-key-mode)
  )

;; --- general.el
(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (general-create-definer space-leader-def
    :states '(normal visual emacs)
    :prefix "SPC")

  (general-create-definer comma-leader-def
    :states '(normal visual emacs)
    :prefix ","
    :keymaps 'override)
  )

;; - bindings

(comma-leader-def
  "," 'hydra-everything/body
  "g" 'hydra-smerge/body
  "s" 'hydra-color-rg/body
  )

(space-leader-def
  "TAB" 'projectile-persp-switch-project
  "SPC" 'execute-extended-command
  "," 'delete-window
  "." 'kill-this-buffer
  ";" 'kill-other-window-buffer
  "`" 'multi-vterm-project
  "x" 'scratch-buffer

  ;; - apps
  "a" '(:ignore t :which-key "apps")
  "aa" 'org-agenda

  ;; - files
  "f" '(:ignore t :which-key "files")
  "ff" '(find-file :which-key "find file")

  ;; - buffer
  "b" '(:ignore t :which-key "buffers")
  "bb" 'consult-buffer
  "bd" 'kill-current-buffer
  "bk" 'kill-buffer
  "bn" 'next-buffer
  "bo" 'consult-buffer-other-window
  "bp" 'previous-buffer

  ;; - open http://baidu.com
  "o" '(:ignore t :which-key "open")
  "om" 'play-sound-file
  "ol" 'link-hint-open-link
  "oc" 'link-hint-copy-link

  ;; - projectile
  "p" '(:ignore t :which-key "projectile")
  "pp" 'consult-projectile-switch-project
  "pf" 'consult-projectile-find-file
  "pd" 'consult-projectile-find-dir
  "pb" 'consult-projectile-switch-to-buffer
  "pl" 'consult-project-buffer

  ;; - search
  "s" '(:ignore t :which-key "search")
  "sf" 'projectile-find-file-in-directory
  "sg" 'rgrep
  "sp" 'consult-ripgrep
  "sr" 'rg
  ;; color-rg
  "si" 'color-rg-search-input-in-project
  "st" 'color-rg-search-symbol-with-type

  ;; - window
  "w" '(:ignore t :which-key "window")
  "w-" 'split-window-below
  "wv" 'split-window-right
  "wm" 'delete-other-windows
  "wh" 'evil-window-left
  "wl" 'evil-window-right
  "wj" 'evil-window-down
  "wk" 'evil-window-up
  )

(general-define-key
 "<s-backspace>" 'crux-kill-line-backwards
 "<s-left>" 'windmove-left
 "<s-right>" 'windmove-right
 "<s-down>" 'windmove-down
 "<s-up>" 'windmove-up

 "s-," 'bury-buffer
 "s-." 'unbury-buffer
 "s-<" 'watch-other-window-up-line
 "s->" 'watch-other-window-down-line
 "s-`" 'vterm-toggle
 "s-'" 'vertico-repeat
 "s-;" 'evilnc-comment-or-uncomment-lines
 "s-d" 'consult-dir
 "s-e" 'transpose-chars
 "s-F" 'format-all-buffer
 "s-i" 'parrot-rotate-next-word-at-point
 "s-m" 'toggle-input-method
 "s-o" 'toggle-one-window
 "s-r" 're-builder
 "s-w" 'transpose-words

 "s-n" 'move-text-down
 "s-p" 'move-text-up

 "M-;" 'evilnc-comment-or-uncomment-lines
 "M-'" 'consult-register-store
 "M-#" 'consult-register-load
 "M-d" 'dash-at-point
 "M-i" 'my-string-inflection-cycle-auto
 "M-s" 'symbol-overlay-put
 "M-n" 'symbol-overlay-switch-forward
 "M-p" 'symbol-overlay-switch-backward
 "M-c" 'symbol-overlay-remove-all
 "M-m" 'blamer-show-posframe-commit-info

 "C-'" 'toggle-quotes-plus
 "C-a" 'crux-move-beginning-of-line
 "C-j" 'emmet-expand-yas
 "C-s" 'consult-line

 "C-c =" 'math-at-point
 "C-c h" 'consult-history
 "C-c o" 'consult-outline
 "C-c r" 'vr/replace
 "C-c q" 'vr/query-replace
 "C-c m" 'vr/mc-mark
 "C-c u" 'uuidgen

 ;; insert
 "C-c iu" 'org-mac-link-get-link
 "C-c is" 'yas-insert-snippet
 "C-c iy" 'consult-yasnippet
 ;; 15:32:24
 "C-c it" 'gcl/insert-current-time
 ;; 2023-08-11 15:32:21
 "C-c id" 'gcl/insert-standard-date
 ;; 2023/08/11
 "C-c il" 'gcl/insert-changelog-date
 "C-c if" 'js-doc-insert-function-doc
 "C-c iF" 'js-doc-insert-file-doc

 ;; bookmark
 "C-c bb" 'consult-bookmark
 "C-c bs" 'bookmark-set
 "C-c cf" 'gcl/copy-file-name-only
 "C-c cp" 'gcl/copy-file-full-name

 ;; yas & fanyi
 "C-c yy" 'fanyi-dwim2
 "C-c yn" 'yas-new-snippet
 "C-c yr" 'yas-reload-all
 "C-c yv" 'yas-visit-snippet-file
 "C-c ys" 'sdcv-search-pointer+
 "C-c yi" 'sdcv-search-input

 "C-c C-'" 'separedit/edit-swagger-commentary
 "C-c C-f" 'devdocs-lookup
 "C-c C-d" 'devdocs-peruse

 "C-S-n" 'duplicate-line-or-region-above
 "C-S-o" 'duplicate-line-or-region-below
 "C-S-h" 'buf-move-left
 "C-S-l" 'buf-move-right
 "C-s-j" 'buf-move-down
 "C-S-k" 'buf-move-up

 "C-c SPC" 'just-one-space
 )

;; 指定模式的按键
(general-define-key
 :keymaps '(evil-normal-state-map evil-motion-state-map evil-insert-state-map)
 "C-r" 'crux-rename-buffer-and-file
 "C-w" 'evil-delete-backward-word
 "C-p" 'previous-line
 "C-n" 'next-line)

;; --- global 按键设置
(global-set-key (kbd "<f5>") 'gcl/reload-init-file)
(global-set-key (kbd "<f1>") 'gcl/open-init-file)
(global-set-key (kbd "<f2>") 'restart-emacs)

;; --- 清理
(defun gcl/cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold  (* 1024 200)) ; 200MB
  (setq gc-cons-percentage 0.5) ; 0.5s
  (garbage-collect))
(run-with-idle-timer 4 nil #'gcl/cleanup-gc)
