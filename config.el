(setq user-full-name "Lee ZhiCheng"
      user-mail-address "ftt.loves@gmail.com")

(defvar gcl/default-font-size 150)
(defvar gcl/default-variable-font-size 150)

;; Make frame transparency overridable
(defvar gcl/frame-transparency '(90 . 90))

(defvar gcl/variable-pitch-size 120)
(defvar gcl/org-heading-font "Iosevka Aile"
  "The font used for Org Mode headings.")

(defun vifon/buffer-file-or-directory-name (buf)
  "The file BUF is visiting, works also if it's a `dired' buffer."
  (with-current-buffer buf
    (or buffer-file-name
	(and (eq major-mode 'dired-mode)
	     (boundp 'dired-directory)
	     (file-name-directory
	      (if (stringp dired-directory)
		  dired-directory
		(car dired-directory)))))))



(defvar toggle-one-window-window-configuration nil
  "The window configuration use for `toggle-one-window'.")

(defun toggle-one-window ()
  "Toggle between window layout and one window."
  (interactive)
  (if (equal (length (cl-remove-if #'window-dedicated-p (window-list))) 1)
      (if toggle-one-window-window-configuration
	  (progn
	    (set-window-configuration toggle-one-window-window-configuration)
	    (setq toggle-one-window-window-configuration nil))
	(message "No other windows exist."))
    (setq toggle-one-window-window-configuration (current-window-configuration))
    (delete-other-windows)))

(global-set-key (kbd "s-o") 'toggle-one-window)

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

(defun gcl/cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold  (* 1024 200)) ; 200MB
  (setq gc-cons-percentage 0.5) ; 0.5s
  (garbage-collect))

(defun switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer (startup--get-buffer-create-scratch)))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(defun gcl/consult-file-externally (file)
  "Open the FILE externally using the system's default program."
  (interactive "File to open externally: ")
  (cond
   ((eq system-type 'darwin) ; macOS
    (start-process "external-program" nil "open" file))
   ((eq system-type 'gnu/linux) ; Linux
    (start-process "external-program" nil "xdg-open" file))
   ((eq system-type 'windows-nt) ; Windows
    (start-process "external-program" nil "start" "" file))
   (t ; Other platforms
    (message "Opening files externally is not supported on this platform."))))


(defun gcl/open-current-directory ()
  (interactive)
  (gcl/consult-file-externally default-directory))

(bind-key [remap just-one-space] #'cycle-spacing)
(bind-key [remap upcase-word] #'upcase-dwim)
(bind-key [remap downcase-word] #'downcase-dwim)
(bind-key [remap capitalize-word] #'capitalize-dwim)
(bind-key [remap count-words-region] #'count-words)
(bind-key [remap eval-last-sexp] #'pp-eval-last-sexp)
(bind-key [remap eval-expression] #'pp-eval-expression)
(bind-key [remap zap-to-char] #'zap-up-to-char)

(global-set-key (kbd "<f5>") 'reload-init-file)

(defun reload-init-file ()
  "重新加载 init.el 文件的函数"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f1>")
                (lambda ()
                  (interactive)
                  (find-file "~/.emacs.d/config.org")))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(when window-system
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (tooltip-mode 0))

;; Bar cursor
(setq-default cursor-type '(bar . 1))
;; 光标不闪烁
(blink-cursor-mode -1)

(setq inhibit-startup-message t
      initial-scratch-message ""
      initial-major-mode 'fundamental-mode
      inhibit-splash-screen t)

;; - 选中粘贴时能覆盖选中的内容
(delete-selection-mode 1)
;; - 高亮当前行
(global-hl-line-mode 1)
(column-number-mode t)
;; 启动全屏
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; - 交换 meta -> option
;;  (when (eq system-type 'darwin)
;;  (setq mac-option-modifier 'meta))
;; - 备份
;; 统一备份到临时文件目录 /tmp/.saves
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "/tmp/.saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups
;; - 自动保存
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      create-lockfiles nil)
;; - 文件编码
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-auto-unix)
;; - 错误信息
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; - 截断行
(setq-default truncate-lines t)

;; - 超大文件警告
(setq large-file-warning-threshold (* 15 1024 1024))

;; - yes or no
(fset 'yes-or-no-p 'y-or-n-p)
;; - 单词自动修正
(setq save-abbrevs 'silently)
;; - ediff
(setq-default abbrev-mode t)
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

;; - ssh
(setq tramp-default-method "ssh"
      tramp-backup-directory-alist backup-directory-alist
      tramp-ssh-controlmaster-options "ssh")

;; - 驼峰单词里移动
(subword-mode)
;; - 默认认为两个空格开头为一个段落，关闭此选项
(setq sentence-end-double-space nil)
;; - 更好的通配符搜索
(setq search-whitespace-regexp ".*?")
;; - 历史记录
(savehist-mode)
;; - C-x n n 开启, C-x n w 关闭
(put 'narrow-to-region 'disabled nil)
;; - PDF 预览
(setq doc-view-continuous t)
;; - 窗口管理
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; - 最近文件
(setq recentf-max-saved-items 1000
      recentf-exclude '("/tmp/" "/ssh:"))
(recentf-mode)

;; - 在 mac 上，当进入一个新的工作空间时，会默认全屏
(setq ns-use-native-fullscreen nil)

;; - 不生成备份文件
(setq make-backup-files nil)

;; 分割窗口的时候自动切换到该窗口
(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting."
  (other-window 1))

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

 ;; 改变 *scratch* buffer 的模式
 initial-major-mode 'emacs-lisp-mode
 initial-buffer-choice t


 )

;; 直接将环境变量拷贝到 ~/.path 中
;; sh -c 'printf "%s" "$PATH"' > ~/.path
(condition-case err
    (let ((path (with-temp-buffer
                  (insert-file-contents-literally "~/.path")
                  (buffer-string))))
      (setenv "PATH" path)
      (setq exec-path (append (parse-colon-path path) (list exec-directory))))
  (error (warn "%s" (error-message-string err))))

(use-package async :commands (async-start))
(use-package cl-lib)
(use-package dash)
(use-package s)

(use-package restart-emacs
  :bind* (("<f2>" . restart-emacs)))

(use-package flyspell
  :diminish (flyspell-mode . "φ")
  :bind* (("M-m ] s" . flyspell-goto-next-error)))

(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "LSP_USE_PLISTS" "NODE_PATH")
        exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(use-package hydra)

(use-package discover-my-major
  :bind (("C-h C-m" . discover-my-major)
         ("C-h s-m" . discover-my-mode)))

(use-package fanyi
  :bind* (("s-y" . fanyi-dwim2))
  :custom
  (fanyi-providers '(;; 海词
                     fanyi-haici-provider
                     ;; 有道同义词词典
                     fanyi-youdao-thesaurus-provider
                     ;; Etymonline
                     fanyi-etymon-provider
                     ;; Longman
                     fanyi-longman-provider)))

;; (setq longman-ins (clone fanyi-longman-provider))
;; (oset longman-ins :word "successful")
;; (fanyi--spawn longman-ins)

;; Wait until *fanyi* buffer has a longman section which means longman-ins has parsed the result.
;;
;; benchmark the render function.
;; (benchmark-run 10 (fanyi-render longman-ins))
;;=> (0.150839075 0 0.0)

(use-package mpv)

(use-package dired
  :straight (:type built-in)
  :bind (:map dired-mode-map
              ("C-c C-e" . wdired-change-to-wdired-mode))
  :init
  (setq dired-dwim-target t
        dired-recursive-copies 'top
        dired-recursive-deletes 'top
        dired-listing-switches "-alh")
  :config
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

(use-package which-key
  :defer t
  :diminish which-key-mode
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha)
  :bind* (("M-m ?" . which-key-show-top-level))
  :config
  (which-key-mode)
  (which-key-add-key-based-replacements
    "M-m ?" "top level bindings")
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 0)
  (setq which-key-idle-secondary-delay 0.05)
  )

(defun exit-modalka ()
  (interactive)
  (modalka-mode 0))

(defun exit-on-space ()
  (interactive)
  (modalka-mode 0)
  (insert-char 32))

(defun exit-on-newline ()
  (interactive)
  (modalka-mode 0)
  (newline-and-indent))

(use-package modalka
  :demand t
  :bind* (("C-z" . modalka-mode))
  :diminish (modalka-mode . "μ")
  :init
  (setq modalka-cursor-type 'box)
  :config
  (global-set-key (kbd "<escape>") #'modalka-mode)
  (modalka-global-mode 1)
  (add-to-list 'modalka-excluded-modes 'magit-status-mode)
  (add-to-list 'modalka-excluded-modes 'magit-popup-mode)
  (add-to-list 'modalka-excluded-modes 'dired-mode)
  (add-to-list 'modalka-excluded-modes 'eshell-mode)
  (add-to-list 'modalka-excluded-modes 'deft-mode)
  (add-to-list 'modalka-excluded-modes 'term-mode)
  (add-to-list 'modalka-excluded-modes 'help-mode)
  (add-to-list 'modalka-excluded-modes 'vterm-mode)
  (which-key-add-key-based-replacements
    "M-m"     "Modalka prefix"
    "M-m :"   "extended prefix"
    "M-m m"   "move prefix"
    "M-m s"   "send code prefix"
    "M-m SPC" "user prefix"
    "M-m g"   "global prefix"
    "M-m o"   "org prefix"
    "M-m a"   "expand around prefix"
    "M-m e"   "buffer edit"
    "M-m i"   "expand inside prefix"
    "M-m ["   "prev nav prefix"
    "M-m ]"   "next nav prefix")
  )

(define-key modalka-mode-map (kbd "o") #'exit-on-newline)
(define-key modalka-mode-map (kbd "i") #'exit-modalka)

(defun auto-enter-modalka-mode ()
  (modalka-mode 1))

(run-with-idle-timer 5 nil 'auto-enter-modalka-mode)

(bind-keys*
 ("C-r"       . dabbrev-expand)
 ("M-/"       . hippie-expand)
 ("C-S-d"     . kill-whole-line)
 ("M-m SPC c" . load-theme)
 ("M-m SPC r" . locate)
 ("M-m w"     . winner-undo)
 ("M-m g m"   . make-frame)
 ("M-m g M"   . delete-frame)
 ("M-m g n"   . select-frame-by-name)
 ("M-m g n"   . set-frame-name)
 ("M-m b"     . mode-line-other-buffer)
 ("M-m ="     . indent-region)
 ("M-m g c"   . upcase-dwim)
 ("M-m g d"   . downcase-dwim)
 ("M-m g f"   . find-file-at-point)
 ("M-m g u"   . downcase-region)
 ("M-m g U"   . upcase-region)
 ("M-m g C"   . capitalize-region)
 ("M-m g F"   . follow-mode)
 ("M-m R"     . overwrite-mode)
 ("M-m : t"   . emacs-init-time)
 ("M-m g @"   . compose-mail)
 ("M-m SPC ?" . describe-bindings)

 ("M-m e l" . duplicate-dwim)

 ("C-c k" . kill-this-buffer)
 )

(bind-keys*
 ;; window move
 ("C-s-h" . windmove-left)
 ("C-s-l" . windmove-right)
 ("C-s-j" . windmove-down)
 ("C-s-k" . windmove-up)
 ;; move buffer
 ("s-H" . buf-move-left)
 ("s-L" . buf-move-right)
 ("s-J" . buf-move-down)
 ("s-K" . buf-move-up)
 )

(modalka-define-kbd "0" "C-x 0")
(modalka-define-kbd "1" "C-1")
(modalka-define-kbd "2" "C-2")
(modalka-define-kbd "3" "C-3")
(modalka-define-kbd "4" "C-4")
(modalka-define-kbd "5" "C-5")
(modalka-define-kbd "6" "C-6")
(modalka-define-kbd "7" "C-7")
(modalka-define-kbd "8" "C-8")
(modalka-define-kbd "9" "C-9")


;; 说明
(which-key-add-key-based-replacements
  "0" "0"
  "1" "1"
  "2" "2"
  "3" "3"
  "4" "4"
  "5" "5"
  "6" "6"
  "7" "7"
  "8" "8"
  "9" "9")

(modalka-define-kbd "c" "M-m g c")	; 单词大写
(modalka-define-kbd "C" "M-m g d")	; 单词小写，M-c 首字母大写
(modalka-define-kbd "h" "C-b")	; 左
(modalka-define-kbd "j" "C-n")	; 下
(modalka-define-kbd "k" "C-p")	; 上
(modalka-define-kbd "l" "C-f")	; 右
(modalka-define-kbd "e" "M-f")	; 移动到单词结尾
(modalka-define-kbd "b" "M-b")	; 移动到单词开头
(modalka-define-kbd "{" "M-{")	; 章节开始
(modalka-define-kbd "}" "M-}")	; 章节结尾
(modalka-define-kbd "0" "C-x 0")	; 行首
(modalka-define-kbd "$" "C-e")	; 行尾
(modalka-define-kbd "G" "M->")	; 文件结尾
(modalka-define-kbd "y" "M-w")	; 复制
(modalka-define-kbd "p" "C-y")	; 粘贴
(modalka-define-kbd "P" "M-y")	; 从粘贴板复制粘贴
(modalka-define-kbd "x" "C-d")	; 删除当前字符
(modalka-define-kbd "D" "C-k")	; 删除光标后的内容
(modalka-define-kbd "z" "C-l")	; 定位中心行
(modalka-define-kbd "!" "M-&")	; 异步执行shell命令
(modalka-define-kbd "J" "C-v")	; 向下翻页
(modalka-define-kbd "K" "M-v")	; 向上翻页
(modalka-define-kbd "(" "M-a")	; 句子开头
(modalka-define-kbd ")" "M-e")	; 句子结尾
(modalka-define-kbd "/" "C-s")	; 文件内搜索
(modalka-define-kbd "E" "C-g")	; 退出模式
(modalka-define-kbd "d" "C-w")	; 删除区域
(modalka-define-kbd "w" "C-x o")	; 切换窗口
(modalka-define-kbd "B" "C-x <left>")	; 上一个buffer
(modalka-define-kbd "N" "C-x <right>"); 下一个buffer
(modalka-define-kbd "u" "C-x u")	; 回退
(modalka-define-kbd "H" "C-x >")	; 向右滚动列
(modalka-define-kbd "L" "C-x <")	; 向左滚动列
(modalka-define-kbd "Z" "C-x 1")	; 关闭其他 buffer
(modalka-define-kbd "q" "C-x (")	; 定制宏
(modalka-define-kbd "Q" "C-x )")	; 退出宏
(modalka-define-kbd "v" "C-SPC")	; 标记
(modalka-define-kbd "?" "M-m ?")	; which-key
(modalka-define-kbd "=" "M-m =")	; 缩进
(modalka-define-kbd "X" "C-x C-x")	; 标记区域光标来回切换
(modalka-define-kbd "+" "C-x r m")	; 书签
(modalka-define-kbd "'" "C-x r b")	; 访问书签
(modalka-define-kbd "\\" "C-c C-c")	; 执行当前光标位置代码
(modalka-define-kbd "," "C-x M-r")	; 显示上一次的搜索结果

(modalka-define-kbd "|" "M-m e l")	; 复制当前行或选中区域

;; 说明
(which-key-add-key-based-replacements
  "ESC" "toggle mode"
  "DEL" "smart del"
  "TAB" "smart tab"
  "RET" "smart enter"
  "c"   "upcase"
  "C"   "downcase"
  "h"   "prev char"
  "j"   "next line"
  "k"   "prev line"
  "l"   "next char"
  "e"   "next word"
  "b"   "prev word"
  "n"   "next history item"
  "N"   "prev history item"
  "{"   "next para"
  "}"   "prev para"
  "0"   "start of line"
  "$"   "end of line"
  "("   "start of sentence"
  ")"   "end of sentence"
  "/"   "search"
  "|"   "duplicate line"
  "E"   "exit anything"
  "B"   "previous buffer"
  "W"   "winner undo"
  "w"   "other window"
  "G"   "end of file"
  "d"   "delete selection"
  "y"   "copy selection"
  "p"   "paste"
  "P"   "paste history"
  "x"   "delete char"
  "D"   "delete rest of line"
  "M"   "modify argument"
  "z"   "scroll center/top/bot"
  "Z"   "zoom into window"
  "H"   "scroll left"
  "J"   "scroll down"
  "K"   "scroll up"
  "L"   "scroll right"
  "'"   "org edit separately"
  ","   "vertico-repeat"
  "q"   "start macro"
  "Q"   "end macro"
  "?"   "top level bindings"
  "v"   "start selection"
  "R"   "overwrite mode"
  "X"   "exchange point and mark"
  "+"   "set bookmark"
  "'"   "jump to bookmark"
  "="   "indent region"
  "\\"  "C-c C-c"
  "!"   "async shell command"
  "&"   "shell command")

(modalka-define-kbd ": q" "C-x C-c")
(modalka-define-kbd ": r" "C-x M-c")
(modalka-define-kbd ": t" "M-m : t")

(modalka-define-kbd "] ]" "C-x n n")
(modalka-define-kbd "] w" "C-x n w")
(modalka-define-kbd "] s" "M-m ] s")

;; 说明
(which-key-add-key-based-replacements
  "]"   "forward nav/edit"
  "] w" "backward nav/edit"
  "] s" "next spell error")

(which-key-add-key-based-replacements
  "["   "backward nav/edit"
  "[ [" "widen region")

(modalka-define-kbd "g U" "C-c C-k")
(modalka-define-kbd "SPC SPC" "M-x")
(modalka-define-kbd "SPC a" "C-x b")
(modalka-define-kbd "s-O" "C-x b")
(modalka-define-kbd "SPC k" "C-x k")
(modalka-define-kbd "SPC g" "M-g g")
(modalka-define-kbd "SPC d" "C-x d")
(modalka-define-kbd "SPC q" "C-x 0")
(modalka-define-kbd "SPC f" "C-x C-f")
(modalka-define-kbd "SPC w" "C-x C-s")
(modalka-define-kbd "SPC c" "M-m SPC c")
(modalka-define-kbd "SPC R" "M-m SPC R")
(modalka-define-kbd "SPC ?" "M-m SPC ?")
(modalka-define-kbd "SPC ." "M-SPC")

;; 说明
(which-key-add-key-based-replacements
  "SPC"   "custom prefix"
  "SPC ." "just one space"
  "SPC ?" "describe bindings"
  "SPC j" "jump to cmd"
  "SPC f" "find file"
  "SPC a" "switch buffers"
  "SPC g" "goto line"
  "SPC d" "dired"
  "SPC k" "close buffer"
  "SPC w" "save buffer"
  "SPC c" "load theme"
  "SPC R" "locate"
  "SPC q" "quit window"
  "g U"   "simulate C-c C-k")

(modalka-define-kbd "g g" "M-<")
(modalka-define-kbd "g o" "C-x C-e")
(modalka-define-kbd "g O" "C-M-x")
(modalka-define-kbd "g m" "M-m g m")
(modalka-define-kbd "g M" "M-m g M")
(modalka-define-kbd "g n" "M-m g n")
(modalka-define-kbd "g N" "M-m g N")
(modalka-define-kbd "g f" "M-m g f")
(modalka-define-kbd "g F" "M-m g F")
(modalka-define-kbd "g j" "M-m g j")
(modalka-define-kbd "g k" "M-m g k")
(modalka-define-kbd "g q" "M-m g q")
(modalka-define-kbd "g w" "C-x 3")
(modalka-define-kbd "g -" "C-x 2")
(modalka-define-kbd "g @" "M-m g @")
(modalka-define-kbd "g ;" "M-m g ;")
(modalka-define-kbd "g :" "M-m g :")
(modalka-define-kbd "g #" "M-m g #")
(modalka-define-kbd "g {" "M-m g {")
(modalka-define-kbd "g }" "M-m g }")
(modalka-define-kbd "g (" "M-m g (")
(modalka-define-kbd "g )" "M-m g )")
(modalka-define-kbd "g S" "C-j")
(modalka-define-kbd "g ?" "C-h k")

;; consult
(modalka-define-kbd "g i" "M-g i")
(modalka-define-kbd "g r" "M-g r")

;; edit
(modalka-define-kbd "g l" "M-g M-g")	; goto line
;; 说明

(which-key-add-key-based-replacements
  "g"   "global prefix"
  "g i" "consult imenu"
  "g r" "consult ripgrep"
  "g g" "start of file"
  "g m" "make frame"
  "g M" "delete frame"
  "g n" "select frame by name"
  "g N" "name frame"
  "g j" "next pdf page"
  "g k" "previous pdf page"
  "g f" "file/url at cursor"
  "g F" "enable follow mode"
  "g l" "goto line"
  "g o" "eval elisp"
  "g O" "eval defun"
  "g w" "vertical split win"
  "g -" "horizontal split win"
  "g S" "split line"
  "g @" "compose mail"
  "g #" "list eww histories"
  "g x" "browse with eww"
  "g :" "browse with external browser"
  "g {" "eww back"
  "g }" "eww forward"
  "g (" "info previous"
  "g )" "info next"
  "g q" "format para"
  "g ?" "find command bound to key")

;; (modalka-define-kbd "i a" "C-x h")

;; (which-key-add-key-based-replacements
;;  "i"   "expand prefix"
;;  "i a" "expand entire buffer")

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

(defun sk/diminish-org-indent ()
  (interactive)
  (diminish 'org-indent-mode ""))
(add-hook 'org-indent-mode-hook 'sk/diminish-org-indent)

(defun sk/diminish-auto-revert ()
  (interactive)
  (diminish 'auto-revert-mode ""))
(add-hook 'auto-revert-mode-hook 'sk/diminish-auto-revert)

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package vertico
  :bind (("C-x M-r" . vertico-repeat)
         :map vertico-map
         ("C-l" . vertico-directory-delete-word)
         ("M-g" . vertico-multiform-grid)
         ("M-q" . vertico-multiform-flat))
  :init (vertico-mode 1)
  :config (progn
            (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
            (vertico-mouse-mode 1)
            (vertico-multiform-mode 1)
            (setq vertico-multiform-categories '((consult-grep buffer))
                  vertico-multiform-commands '((tmm-menubar flat)
                                               (tmm-shortcut flat)))

            ;; Needed with `read-file-name-completion-ignore-case'.
            ;; See these links:
            ;; - https://github.com/minad/vertico/issues/341
            ;; - https://debbugs.gnu.org/cgi/bugreport.cgi?bug=60264
            ;;
            ;; Regardless of it fixing an actual bug, I prefer
            ;; this behavior.
            (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))
  )

(use-package orderless
  :demand t
  :config (progn
            (setq orderless-matching-styles '(orderless-regexp
                                              orderless-initialism
                                              orderless-prefixes)
                  orderless-component-separator #'orderless-escapable-split-on-space)

            ;; Use the built-in "partial-completion" style to complete
            ;; file inputs such as "/e/ni/co.nix" into
            ;; "/etc/nixos/configuration.nix".
            (setq completion-category-defaults nil
                  completion-category-overrides '((file (styles partial-completion))))

            ;; Make the stock file completion styles ("basic" and
            ;; "partial-completion") case insensitive, it fits better
            ;; with the behavior provided by orderless.  See the
            ;; `orderless-smart-case' documentation for how it
            ;; interacts with orderless itself (spoiler: in this setup
            ;; it doesn't).
            (setq read-file-name-completion-ignore-case t)

            (setq completion-styles '(orderless basic))

            (defun vifon/call-without-orderless-dispatchers (orig &rest args)
              "Use with `advice-add' (`:around') to ignore the dispatchers."
              (let ((orderless-style-dispatchers nil))
                (apply orig args)))))

(use-package embark
  :bind (("C-c o" . embark-dwim)
         ("C-."   . embark-act)
         :map minibuffer-local-map
         ("M-o"   . embark-act)
         :map embark-command-map
         ;; Unbind the dangerous `global-set-key' and `local-set-key'
         ;; actions.  It's far too easy to accidentally bind over some
         ;; `self-insert-command' binding or even over
         ;; \\[keyboard-quit].
         ("g" . nil)
         ("l" . nil)
         :map embark-collect-mode-map
         ("m" . vifon/embark-select-and-forward))
  :init
  ;; Optionally replace the key help with a completing-(region-end)ad interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config (progn
            (setq embark-mixed-indicator-delay 2)

            ;; Make the eval action editable.  Evaluating code
            ;; in-place is simple enough without Embark, if I invoke
            ;; it with Embark, I almost definitely want to edit the
            ;; expression beforehand.  And even if not, I can
            ;; just confirm.
            (cl-pushnew 'embark--allow-edit
                        (alist-get 'pp-eval-expression embark-target-injection-hooks))

            ;; Reload the project list after using
            ;; C-u `embark-act' with `project-forget-project'.
            (cl-pushnew 'embark--restart
                        (alist-get 'project-forget-project embark-post-action-hooks))

            (defun embark-act-with-eval (expression)
              "Evaluate EXPRESSION and call `embark-act' on the result."
              (interactive "sExpression: ")
              (with-temp-buffer
                (let ((expr-value (eval (read expression))))
                  (insert (if (stringp expr-value)
                              expr-value
                            (format "%S" expr-value))))
                (embark-act)))

            (dolist (keymap (list embark-variable-map embark-expression-map))
              (define-key keymap (kbd "v") #'embark-act-with-eval))

            ;; Source: https://github.com/oantolin/embark/wiki/Additional-Actions#attaching-file-to-an-email-message
            (autoload 'gnus-dired-attach "gnus-dired" nil t)
            (defun embark-attach-file (file)
              "Attach FILE to an email message."
              (interactive "fAttach: ")
              (cl-letf (((symbol-function 'y-or-n-p) #'always))
                (gnus-dired-attach (list file))))
            (bind-key "a" #'embark-attach-file embark-file-map)

            (defun vifon/embark-select-and-forward ()
              (interactive)
              (embark-select)
              (forward-button 1))))


(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :after vertico
  :demand t                     ; :demand applies to :bind but not
                                        ; :after.  We want to eagerly load
                                        ; marginalia once vertico is loaded.
  :bind (:map minibuffer-local-map
              ("C-o" . marginalia-cycle))
  :config (marginalia-mode 1))

(use-package consult
  :bind (:map consult-mode-map
              ;; M-s …
              ("M-s u" . consult-focus-lines)
              ("M-s k" . consult-keep-lines)
              ("M-s e" . consult-isearch-history)
              ("M-s d" . consult-find)
              ;; M-g …
              ("M-g g" . consult-line)
              ("M-g o" . consult-outline)
              ("C-s-s" . consult-outline)
              ("M-g i" . consult-imenu)
              ("M-g I" . consult-info)
              ("M-g r" . consult-ripgrep)
              ("<f6>" . consult-ripgrep)
              ("M-g m" . consult-mark)
              ("M-g M" . consult-global-mark)
              ;; Misc.
              ("C-x C-r" . consult-recent-file)
              ;; Remaps
              ([remap switch-to-buffer]              . consult-buffer)
              ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
              ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
              ([remap project-switch-to-buffer]      . consult-project-buffer)
              ([remap yank-pop]                      . consult-yank-pop)
              ([remap goto-line]                     . consult-goto-line)
              ([remap bookmark-jump]                 . consult-bookmark)
              ([remap repeat-complex-command]        . consult-complex-command)
              ([remap isearch-forward]               . consult-line)
              ;; Remaps for `Info-mode'.
              ([remap Info-search] . consult-info)

              :map isearch-mode-map
              ("TAB" . consult-line))
  :init (progn
          (defvar consult-mode-map (make-sparse-keymap))
          (define-minor-mode consult-mode
            "Provide the `consult' commands in a single keymap."
            :global t
            (if consult-mode
                (define-key minibuffer-local-map
                            [remap previous-matching-history-element]
                            #'consult-history)
              (define-key minibuffer-local-map
                          [remap previous-matching-history-element]
                          nil)))
          (consult-mode 1))
  :config (progn
            (consult-customize
             consult-ripgrep consult-grep
             consult-buffer consult-recent-file
             :preview-key "M-.")

            (defun vifon/orderless-fix-consult-tofu (pattern index total)
              "Ignore the last character which is hidden and used only internally."
              (when (string-suffix-p "$" pattern)
                `(orderless-regexp . ,(concat (substring pattern 0 -1)
                                              "[\x200000-\x300000]*$"))))

            (dolist (command '(consult-buffer consult-line))
              (advice-add command :around
                          (lambda (orig &rest args)
                            (let ((orderless-style-dispatchers (cons #'vifon/orderless-fix-consult-tofu
                                                                     orderless-style-dispatchers)))
                              (apply orig args)))))

            ;; Disable consult-buffer project-related capabilities as
            ;; they are very slow in TRAMP.
            (setq consult-buffer-sources
                  (delq 'consult--source-project-buffer
                        (delq 'consult--source-project-file consult-buffer-sources)))

            (setq consult--source-hidden-buffer
                  (plist-put consult--source-hidden-buffer :narrow ?h))

            (defvar vifon/consult--source-disassociated-file-buffer
              `(:name     "Disassociated File"
                          :narrow   ?e
                          :category buffer
                          :state    ,#'consult--buffer-state
                          :items
                          ,(lambda ()
                             (consult--buffer-query :sort 'visibility
                                                    :as #'buffer-name
                                                    :predicate
                                                    (lambda (buf)
                                                      (let ((file (vifon/buffer-file-or-directory-name buf)))
                                                        (and file (not (file-exists-p file)))))))
                          "Disassociated buffer candidate source for `consult-buffer'.

Inspired by: `ibuffer-mark-dissociated-buffers'."))
            (defun vifon/consult-disassociated-buffers ()
              "Like `consult-buffer' but only for disassociated buffers."
              (interactive)
              (consult-buffer '(vifon/consult--source-disassociated-file-buffer)))


            (defvar vifon/consult--source-remote-file-buffer
              `(:name     "Remote File"
                          :narrow   ?r
                          :hidden   t
                          :category buffer
                          :state    ,#'consult--buffer-state
                          :items
                          ,(lambda ()
                             (consult--buffer-query :sort 'visibility
                                                    :as #'buffer-name
                                                    :predicate
                                                    (lambda (buf)
                                                      (let ((file (vifon/buffer-file-or-directory-name buf)))
                                                        (and file (file-remote-p file))))))
                          "Remote file buffer candidate source for `consult-buffer'."))
            (add-to-list 'consult-buffer-sources
                         'vifon/consult--source-remote-file-buffer
                         'append)

            ;; Use Consult to select xref locations with preview.
            (setq xref-show-xrefs-function #'consult-xref
                  xref-show-definitions-function #'consult-xref)

            (add-to-list 'consult-bookmark-narrow
                         '(?t "TMSU" tmsu-dired-bookmark-open))))

(use-package corfu
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)
              ("TAB" . corfu-insert)
              ([tab] . corfu-insert)
              ("C-f" . corfu-insert))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-preview-current nil)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)

  :config
  (global-corfu-mode 1)

  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

(use-package kind-icon
  :after corfu
  :custom (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package magit
  :bind* (("M-m g d" . magit))
  :config
  ;; ;; 提交时候不显示提交细节
  (setq magit-commit-show-diff nil)
  ;; ;; 没有焦点时候不刷新状态
  (setq magit-refresh-status-buffer nil)
  ;; ;; 当前buffer打开magit
  (setq magit-display-buffer-function
        (lambda (buffer)
          (display-buffer buffer '(display-buffer-same-window))))
  ;; (setq magit-ellipsis (get-byte 0 "."))
  ;; ;; 加速diff
  (setq magit-revision-insert-related-refs nil)
  (setq magit-diff-refine-hunk t)
  (setq magit-diff-paint-whitespace nil)
  (setq magit-ediff-dwim-show-on-hunks t)
  (setq magit-display-buffer-function
        (lambda (buffer)
          (display-buffer buffer '(display-buffer-same-window))))
  ;; ;; 加速diff
  (setq magit-revision-insert-related-refs nil)
  )

(modalka-define-kbd "g d" "M-m g d")
(which-key-add-key-based-replacements "g d" "magit")

(use-package blamer
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 40)
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
  :config
  (add-to-list 'auto-mode-alist
               (cons "/.dockerignore\\'" 'gitignore-mode))
  (add-to-list 'auto-mode-alist
               (cons "/.gitignore\\'" 'gitignore-mode))
  (add-to-list 'auto-mode-alist
               (cons "/.gitconfig\\'" 'gitconfig-mode))
  )

(use-package smerge-mode)

(use-package diff-hl
  :config
  (global-diff-hl-mode))

(defhydra gcl/smerge-panel ()
  "smerge"
  ("k" (smerge-prev) "prev change" )
  ("j" (smerge-next) "next change")
  ("u" (smerge-keep-upper) "keep upper")
  ("l" (smerge-keep-lower) "keep lower")
  ("q" nil "quit" :exit t))

(use-package expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

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
  :bind* (("M-m e i" . maple/iedit/body)))

(modalka-define-kbd "C-," "M-m e i")

(which-key-add-key-based-replacements
  "C-," "iedit"
  )

(use-package move-text)
(global-set-key (kbd "s-<") 'move-text-up)
(global-set-key (kbd "s->") 'move-text-down)

(use-package string-inflection)
(global-set-key (kbd "s-i") 'my-string-inflection-cycle-auto)

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
          ;; (:rot ("get" "set") :caps t :upcase t)
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
          (:rot ("sm" "md" "lg" "xl" "2xl" "3xl"))
          (:rot ("aspect-auto" "aspect-square" "aspect-video"))
          (:rot ("break-after-auto" "break-after-avoid" "break-after-all" "break-after-avoid-page" "break-after-page" "break-after-left" "break-after-right" "break-after-column"))
          (:rot ("box-border" "box-content"))
          (:rot ("block" "inline-block" "inline" "flex" "inline-flex" "table" "inline-table" "table-caption" "table-cell" "table-column" "table-column-group" "table-footer-group" "table-header-group" "table-row-group" "table-row" "flow-root" "grid" "inline-grid" "contents" "list-item" "hidden"))
          (:rot ("float-right" "float-left" "float-none"))
          (:rot ("clear-left" "clear-right" "clear-both" "clear-none"))
          (:rot ("object-contain" "object-cover" "object-fill" "object-none" "object-scale-down"))
          (:rot ("object-bottom" "object-center" "object-left" "object-left-bottom" "object-left-top" "object-right" "object-right-bottom" "object-right-top" "object-top"))
          (:rot ("overflow-auto" "overflow-hidden" "overflow-clip" "overflow-visible" "overflow-scroll"))
          (:rot ("static" "fixed" "absolute" "relative" "sticky"))
          (:rot ("visible" "invisible" "collapse"))
          (:rot ("flex-row" "flex-row-reverse" "flex-col" "flex-col-reverse"))
          (:rot ("flex-wrap" "flex-wrap-reverse" "flex-nowrap"))
          (:rot ("flex-1" "flex-auto" "flex-initial" "flex-none"))
          (:rot ("grow" "grow-0"))
          (:rot ("shrink" "shrink-0"))
          (:rot ("get" "post" "set") :caps t :upcase t)
          )))

(global-set-key (kbd "s-I") 'parrot-rotate-next-word-at-point)

(use-package toggle-quotes-plus
  :straight (toggle-quotes-plus :type git :host github :repo "jcs-elpa/toggle-quotes-plus")
  :bind* (("C-'" . toggle-quotes-plus))
  :config
  (setq toggle-quotes-plus-chars '("\""
                                   "'"
                                   "`")))

(set-face-attribute 'default nil :font "Fira Code Retina" :height gcl/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height gcl/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height gcl/default-variable-font-size :weight 'regular)

(use-package all-the-icons)
(use-package all-the-icons-dired
  :diminish all-the-icons-dired-mode
  :hook ((dired-mode . all-the-icons-dired-mode)))

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

;; Must be used *after* the theme is loaded
;; (custom-set-faces
;; `(mode-line ((t (:background ,(doom-color 'dark-violet)))))
;; `(font-lock-comment-face ((t (:foreground ,(doom-color 'base6))))))
;; 设置默认字体为等宽字体
;; (set-face-attribute 'default nil
;; 		    :family "Fira Code"
;; 		    :height 130
;; 		    :weight 'normal
;; 		    :width 'normal)

(use-package doom-modeline
  :init (progn
          (setq doom-modeline-env-version nil
                doom-modeline-icon nil
                doom-modeline-minor-modes t)
          (doom-modeline-mode 1)))

(when (version<= "9.2" (org-version))
  (require 'org-tempo))

(setq org-directory "~/.gclrc/org")

(defun gcl/org-path (path)
  (expand-file-name path org-directory))

;; Turn on indentation and auto-fill mode for Org files
(defun dw/org-mode-setup ()
  ;; (variable-pitch-mode 1)
  (org-indent-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq corfu-auto nil)
  (setq evil-auto-indent nil))

(defun dw/org-move-done-tasks-to-bottom ()
  "Sort all tasks in the topmost heading by TODO state."
  (interactive)
  (save-excursion
    (while (org-up-heading-safe))
    (org-sort-entries nil ?o))

  ;; Reset the view of TODO items
  (org-overview)
  (org-show-entry)
  (org-show-children))


(defun dw/org-todo-state-change-hook ()
  (when (string= org-state "DONE")
    (dw/org-move-done-tasks-to-bottom)))
;; (add-hook 'org-after-todo-state-change-hook 'dw/org-todo-state-change-hook)
(use-package org
  :straight (:type built-in)
  :hook (org-mode . dw/org-mode-setup)
  :bind (:map org-mode-map
              ("M-n" . org-move-subtree-down)
              ("M-p" . org-move-subtree-up))
  :config
  (setq org-ellipsis "..."
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2
        org-capture-bookmark nil
        )

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)))
  )

(use-package org-faces
  :straight (:type built-in)
  :after org
  :config
  ;; Increase the size of various headings
  (set-face-attribute 'org-document-title nil :font gcl/org-heading-font :weight 'medium :height 1.3)
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font gcl/org-heading-font :weight 'medium :height (cdr face))))

(use-package org-modern
  :hook (org-mode . org-modern-mode))

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

(define-key org-mode-map (kbd "s-t") 'org-todo)

(use-package posframe)

(use-package yasnippet
  :diminish yas-minor-mode
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :bind (("C-c y i" . yas-insert-snippet)
         ("C-c y f" . yas-visit-snippet-file)
         ("C-c y n" . yas-new-snippet)
         ("C-c y t" . yas-tryout-snippet)
         ("C-c y l" . yas-describe-tables)
         ("C-c y g" . yas-global-mode)
         ("C-c y m" . yas-minor-mode)
         ("C-c y r" . yas-reload-all)
         ("C-c y x" . yas-expand)
         :map yas-keymap
         ("C-i" . yas-next-field-or-maybe-expand))
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :defer t
  :after yasnippet)

(use-package evil-nerd-commenter
  :bind* (("M-;" . evilnc-comment-or-uncomment-lines))
  )

(use-package yaml-mode
  :mode "\\.yml\\'"
  :mode "\\.yaml\\'"
  :hook ((yaml-mode . yaml-imenu-enable)))

(use-package yaml-imenu
  :after yaml-mode)

(use-package python-mode)

(use-package go-mode)

(use-package dockerfile-mode)

(use-package php-mode)

(use-package sql-indent)
(add-hook 'sql-mode-hook 'sqlind-minor-mode)

(use-package pkg-info)

(use-package lua-mode)

(use-package flycheck
  :diminish flycheck-mode
  :init (global-flycheck-mode))

(use-package auto-save
  :straight (auto-save :type git :host github :repo "manateelazycat/auto-save")
  :config
  (auto-save-enable)
  (setq auto-save-silent t)
  (setq auto-save-delete-trailing-whitespace t))

(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode)
  :diminish highlight-parentheses-mode
  :config
  (add-hook 'minibuffer-setup-hook #'highlight-parentheses-minibuffer-setup)
  )

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-mode
  :diminish rainbow-mode
  :defer t
  :hook ((prog-mode org-mode) . rainbow-mode))

(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c TAB"))  ; pick your own prefix key here
  :init
  (persp-mode)
  :diminish perps-mode
  :config
  (setq persp-state-default-file (expand-file-name ".gcl" user-emacs-directory))
  (setq persp-show-modestring nil)
  ;; (setq persp-modestring-short t)
  (add-hook 'kill-emacs-hook #'persp-state-save)
  (use-package persp-projectile)
  )

(use-package projectile
  :diminish projectile-mode
  :init
  (projectile-mode +1)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;; alien, hybrid
  (setq projectile-indexing-method 'alien projectile-enable-caching t)
  )

(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
                         :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                         :build (:not compile))
  :init
  (global-lsp-bridge-mode)
  :diminish lsp-bridge-mode
  :config
  (require 'lsp-bridge-jdtls)
  ;; (require 'acm-backend-tailwind)
  (setq lsp-bridge-python-command "/usr/bin/python3")
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


  ;; 打开日志，开发者才需要
  (setq lsp-bridge-enable-log nil)

  (setq acm-backend-lsp-candidate-min-length 2)
  (setq acm-backend-elisp-candidate-min-length 2)
  (setq acm-backend-yas-candidate-min-length 1)
  (setq acm-backend-codeium-candidate-min-length 2)
  ;; --- deno
  ;; (setq lsp-bridge-get-single-lang-server-by-project
  ;;       (lambda (project-path filepath)
  ;;         ;; If typescript first line include deno.land, then use Deno LSP server.
  ;;         (save-excursion
  ;;           (when (string-equal (file-name-extension filepath) "ts")
  ;;             (dolist (buf (buffer-list))
  ;;               (when (string-equal (buffer-file-name buf) filepath)
  ;; 		            (with-current-buffer buf
  ;;                   (goto-char (point-min))
  ;;                   (when (string-match-p (regexp-quote "from \"https://deno.land") (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
  ;;                     (return "deno")))))))))
  )

(use-package typescript-mode
  :mode "\\.[cm]?ts\\'"
  ;; :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package json-mode)

(use-package scss-mode)

(use-package css-mode)

(use-package emmet-mode
  :diminish emmet-mode
  :hook ((sgml-mode html-mode css-mode web-mode typescript-mode js-mode) . emmet-mode)
  :config
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))))

(use-package web-mode
  ;; :hook (web-mode . lsp-deferred)
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

  ;; 设置不同类型代码的注释格式
  (setq web-mode-comment-formats
	'(("javascript" . "//")    ; JavaScript 注释
	  ("jsx" . "//")           ; JSX 注释
	  ("php" . "//")           ; PHP 注释
	  ("css" . "/*")           ; CSS 注释
	  ("java" . "//")          ; Java 注释
	  ;; 添加更多类型的注释格式
	  ))

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
  :config
  (setq js-doc-mail-address user-mail-address
	js-doc-author (format "<%s> <%s>" user-full-name js-doc-mail-address)
	;; js-doc-url user-blog-url
	;; js-doc-license "MIT"
	))

(defun dw/setup-markdown-mode ()
  (visual-fill-column-mode 1)
  (display-line-numbers-mode 0))

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked")
  (add-hook 'markdown-mode-hook #'dw/setup-markdown-mode)
  (dolist (face '((markdown-header-face-1 . 1.2)
                  (markdown-header-face-2 . 1.1)
                  (markdown-header-face-3 . 1.0)
                  (markdown-header-face-4 . 1.0)
                  (markdown-header-face-5 . 1.0)))
    (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (sp-use-smartparens-bindings))

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
                    ("\\.prettierrc" . yaml-mode)
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

;; (add-to-list 'interpreter-mode-alist '("coffee" . coffee-mode))
;; (add-hook 'web-mode-hook 'emmet2-mode)


;; 自动换行
;; (dolist (hook (list
;;                'after-text-mode-hook
;;                'message-mode-hook
;;                ))
;;   (add-hook hook #'(lambda () (auto-fill-mode 1))))

(use-package vterm)
(use-package multi-vterm)
(use-package vterm-toggle)
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

(global-set-key (kbd "C-`") 'vterm-toggle)
(global-set-key (kbd "s-<return>") 'multi-vterm-project)

(use-package engine-mode
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

(use-package wgrep)
(setq wgrep-auto-save-buffer t)

(use-package visual-regexp)
  (use-package visual-regexp-steroids)
(global-set-key (kbd "C-c r") 'vr/replace)
(global-set-key (kbd "C-c q") 'vr/query-replace)
(global-set-key (kbd "C-c m") 'vr/mc-mark)

(defun tangle-if-init ()
  "If the current buffer is 'init.org' the code-blocks are
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
