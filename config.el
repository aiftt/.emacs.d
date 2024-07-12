(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq user-mail-address "ftt.loves@gmail.com")
(setq user-full-name "Lee ZhiCheng")

(defun get-project-root ()
  "Get the root directory of the current project."
  (or (locate-dominating-file default-directory "package.json")
      (locate-dominating-file default-directory ".git")))

(defun get-eslint-executable ()
  "Get the path to the eslint executable in the current project."
  (let ((root (get-project-root)))
    (if root
        (expand-file-name "node_modules/.bin/eslint" root)
      "eslint")))  ;; 默认使用全局 eslint

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

(global-set-key (kbd "C-c c n") 'gcl/copy-file-name-only)
(global-set-key (kbd "C-c c p") 'gcl/copy-file-full-name)

(defun switch-to-scratch-buffer ()
  "Switch to *scratch* buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(global-set-key (kbd "C-c b s") 'switch-to-scratch-buffer)

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

(global-set-key (kbd "C-c i d") 'gcl/insert-standard-date)
(global-set-key (kbd "C-c i t") 'gcl/insert-current-time)

(defun gcl/consult-file-externally (file)
  "Open the FILE externally using the system's default program."
  (interactive "fFile to open externally: ")
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

  (global-set-key (kbd "C-c b f") 'gcl/open-current-directory)

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

(defvar gcl/default-font-size 150)
(defvar gcl/default-variable-font-size 150)

;; Make frame transparency overridable
(defvar gcl/frame-transparency '(90 . 90))

(defvar gcl/variable-pitch-size 120)
(defvar gcl/org-heading-font "Iosevka Aile"
  "The font used for Org Mode headings.")

(global-unset-key (kbd "s-g"))

;; 光标样式
(setq-default cursor-type '(bar . 1))
;; 光标不闪烁
(blink-cursor-mode -1)

;; 关闭 Edebug 日志输出
(setq edebug-trace nil)

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
;; - 窗口管理
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; - 在 mac 上，当进入一个新的工作空间时，会默认全屏
(setq ns-use-native-fullscreen nil)

;; - 不生成备份文件
(setq make-backup-files nil)

;; 启用自动保存已访问的文件 ss
;; (auto-save-visited-mode 1)
;; 设置自动保存的间隔时间
;; (setq auto-save-visited-interval 1)  ; 每秒钟保存一次当前文件的备份
;; (setq auto-save-interval 1)          ; 每秒钟保存一次所有文件的备份
(setq save-silently t)  ; 自动保存文件，避免提示确认
;; 分割窗口的时候自动切换到该窗口
;; (add-hook 'window-setup-hook 'select-window)


;; 有些功能需要用到，比如：折叠等等
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; (defun my-split-window-and-switch ()
;;   "Split the window and switch to the newly created window."
;;   (interactive)
;;   (let ((current-window (selected-window)))
;;     (call-interactively #'split-window)
;;     (select-window (next-window current-window))))

;; (advice-add 'split-window :after #'my-split-window-and-switch)

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

(use-package autorevert
:init
(global-auto-revert-mode)
(setq auto-revert-verbose nil))

(defun sk/diminish-auto-revert ()
  (interactive)
  (diminish 'auto-revert-mode ""))
(add-hook 'auto-revert-mode-hook 'sk/diminish-auto-revert)

;; TODO

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
              ("M-N" . org-move-subtree-down)
              ("M-P" . org-move-subtree-up)
              ("M-`" . org-overview))
  :config
  (setq org-ellipsis "..."
        org-imenu-depth 4 ; 可搜索的标题层级
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

  ;; 重新生成 org-imenu 索引
  (add-hook 'org-mode-hook
            (lambda ()
              (setq imenu-create-index-function 'org-imenu-get-tree)))
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

(use-package org-modern
  :hook (org-mode . org-modern-mode))

(use-package async :commands (async-start))
(use-package cl-lib)
(use-package dash)
(use-package s)
(use-package hydra)

(use-package dashboard
  :init
  (setq dashboard-items '((recents  . 11)
                          (bookmarks . 5)
                          (registers . 5))
        dashboard-banner-logo-title "我总在不经意之间觉得自己很傻比🤪🤪🤪!"
        dashboard-startup-banner 'official)
  :config
  (dashboard-setup-startup-hook))

(use-package which-key
  :defer t
  :diminish which-key-mode
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha)
  :bind* (("M-m ?" . which-key-show-top-level))
  :config
  (which-key-mode)
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 0)
  (setq which-key-idle-secondary-delay 0.05)
  )

(use-package symbol-overlay
  :diminish symbol-overlay-mode
  :config
  (symbol-overlay-mode +1)
  (global-set-key (kbd "M-i") #'symbol-overlay-put)
  (global-set-key (kbd "M-n") #'symbol-overlay-switch-forward)
  (global-set-key (kbd "M-p") #'symbol-overlay-switch-backward)
  (global-set-key (kbd "<f7>") #'symbol-overlay-mode)
  (global-set-key (kbd "<f8>") #'symbol-overlay-remove-all)
  )

(use-package toggle-quotes-plus
  :straight (toggle-quotes-plus :type git :host github :repo "jcs-elpa/toggle-quotes-plus")
  :bind* (("C-'" . toggle-quotes-plus))
  :config
  (setq toggle-quotes-plus-chars '("\""
                                   "'"
                                   "`")))

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

(use-package move-text)
(global-set-key (kbd "s-<") 'move-text-up)
(global-set-key (kbd "s->") 'move-text-down)

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

(use-package expreg
  :bind (("C-=" . expreg-expand)))

(use-package diminish
  :demand t
  :diminish org-indent-mode
  :diminish visual-line-mode
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

(use-package all-the-icons)
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :functions
  all-the-icons-completion-mode
  :hook
  (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package all-the-icons-dired
  :diminish)

(use-package all-the-icons-ibuffer
  :after (ibuffer)
  :functions
  all-the-icons-ibuffer-mode
  :config
  (all-the-icons-ibuffer-mode 1))

(use-package discover-my-major
  :bind (("C-h C-m" . discover-my-major)
         ("C-h s-m" . discover-my-mode)))

(use-package visual-fill-column)

(use-package avy
  :bind
  ("s-g x" . avy-copy-line) ; 将指定行复制到光标位置
  ("s-g m" . avy-move-line) ; 将指定行移动到光标位置
  ("s-g w" . avy-goto-word-or-subword-1)
  ("s-g l" . avy-goto-line)
  ("s-g c" . avy-goto-char)
  )

(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "s-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-z") 'undo-fu-only-redo))

(use-package duplicate-thing
  :straight (duplicate-thing :type git :host github :repo "artemkovalyov/duplicate-thing")
  :bind
  ("C-S-l" . duplicate-thing))

(use-package smart-hungry-delete
  :bind (([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
         ([remap delete-backward-char] . smart-hungry-delete-backward-char)
         ([remap org-delete-backward-char] . smart-hungry-delete-backward-char)
         ([remap delete-char] . smart-hungry-delete-forward-char))
  :init (smart-hungry-delete-add-default-hooks))

;; (global-set-key (kbd "<backspace>") 'smart-hungry-delete-backward-char)
;; (global-set-key (kbd "<delete>") 'smart-hungry-delete-backward-char)
;; (global-set-key (kbd "C-d") 'smart-hungry-delete-forward-char)

(use-package windmove
  :bind
  ("s-h" . windmove-left)
  ("s-l" . windmove-right)
  ("s-j" . windmove-down)
  ("s-k" . windmove-up)
  ("s-w" . delete-other-windows)
  ("s-q" . delete-window)
  ("s-0" . delete-window)
  ;; ("A-s-i" . enlarge-window)
  ;; ("A-s-k" . shrink-window)
  ;; ("A-s-j" . shrink-window-horizontally)
  ;; ("A-s-l" . enlarge-window-horizontally)
  ("s--" . split-window-horizontally)
  ("s-=" . split-window-vertically)
  )

(use-package crux)
(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
(global-set-key [remap org-beginning-of-line] #'crux-move-beginning-of-line)
(global-set-key [remap org-table-copy-down] #'crux-smart-open-line)
(global-set-key (kbd "C-c o") #'crux-open-with)
(global-set-key [(shift return)] #'crux-smart-open-line)
(global-set-key (kbd "s-r") #'crux-recentf-find-file)
(global-set-key (kbd "C-s-<backspace>") #'crux-kill-line-backwards)
(global-set-key (kbd "C-k") #'crux-smart-kill-line)
(global-set-key (kbd "C-c u") #'crux-view-url)
(global-set-key (kbd "C-x C-w") #'crux-transpose-windows)
(global-set-key (kbd "C-c D") #'crux-delete-file-and-buffer)
(global-set-key (kbd "C-c R") #'crux-rename-file-and-buffer)
(global-set-key (kbd "C-c b K") #'crux-kill-other-buffers)

(use-package youdao-dictionary
  :bind (("C-c y ." . youdao-dictionary-search-at-point+)
         ("C-c y ," . youdao-dictionary-search-from-input)
         ("C-c yv" . youdao-dictionary-play-voice-at-point))
  :config
  ;; Enable Cache
  (setq url-automatic-caching t))

(use-package fanyi
  :bind (("C-c y y" . fanyi-dwim2)
         ("C-c y h" . fanyi-from-history))
  :config
  ;; 不自动跳转到翻译窗口
  ;; (setq fanyi-auto-select nil)
  :custom
  (fanyi-providers '(;; 海词
                     fanyi-haici-provider
                     ;; 有道同义词词典
                     fanyi-youdao-thesaurus-provider
                     ;; Etymonline
                     fanyi-etymon-provider
                     ;; Longman
                     fanyi-longman-provider)))

(use-package buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

(use-package goto-chg
  :bind (("M--" . goto-last-change)
         ("M-=" . goto-last-change-reverse)
         ))

(use-package embrace
  :bind
  (("C-q" . embrace-commander))
  :config
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
    ;; 在 `prog-mode` 和 `text-mode` 中启用 `embrace`
  (add-hook 'prog-mode-hook 'embrace-enable)
  (add-hook 'text-mode-hook 'embrace-enable)
  )

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

(defun mc/my-quit ()
  "Quit from mark mode."
  (interactive)
  (mc/keyboard-quit)
  (multiple-cursors-mode 0))

(defun mc/mark-all-symbols-like-this-toggle ()
  "Toogle when only one matches!"
  (interactive)
  (if (region-active-p)
      (mc/my-quit)
    (mc/mark-all-symbols-like-this)))

(use-package multiple-cursors
  :bind (("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-M->"         . mc/skip-to-next-like-this)
         ("C-M-<"         . mc/skip-to-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         ;; ("C-;"           . mc/mark-all-symbols-like-this-toggle)
         ("C-:"           . mc/mark-all-symbols-like-this-in-defun)
         :map mc/keymap
         ("C-|" . mc/vertical-align-with-space)
         ("C-_" . undo)                 ;undo-tree-undo point position wrong.
         ;; ("C-;" . mc/my-quit)
         ("M-n" . mc/cycle-forward)
         ("M-p" . mc/cycle-backward))
  :config
  (setq mc/insert-numbers-default 1))

(use-package visual-regexp)
(use-package visual-regexp-steroids)
(global-set-key (kbd "C-c r") 'vr/replace)
(global-set-key (kbd "C-c q") 'vr/query-replace)
(global-set-key (kbd "C-c m") 'vr/mc-mark)

(use-package color-rg
  :straight (:type git :host github :repo "manateelazycat/color-rg")
  :bind (("M-s i" . color-rg-search-input)
         ("M-s s" . color-rg-search-symbol)
         ("M-s M-i" . color-rg-search-input-in-project)
         ("M-s M-s" . color-rg-search-symbol-in-project)
         ("M-s f" . color-rg-search-input-in-current-file)
         ("M-s F" . color-rg-search-symbol-in-current-file)
         ("M-s e" . color-rg-search-symbol-with-type)
         ("M-s M-e" . color-rg-search-project-with-type)))

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
                doom-modeline-minor-modes t
                doom-modeline-buffer-file-name-style 'file-name
                doom-modeline-buffer-encoding nil)
          (doom-modeline-mode 1)))

(use-package perspective
  :bind
  (("<f10>" . persp-switch)
   ("C-<tab>" . persp-switch))
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
  :bind (("<f9>" . projectile-persp-switch-project))
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;; alien, hybrid
  (setq projectile-indexing-method 'alien projectile-enable-caching t)
  )

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ;; ("C-c h" . consult-history)
         ;; ("C-c k" . consult-kmacro)
         ;; ("C-c m" . consult-man)
         ;; ("C-c i" . consult-info)
         ("C-c s p" . consult-ripgrep)
         ([remap Info-search] . consult-info)
         ([remap isearch-forward] . consult-line)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap switch-to-buffer] . consult-buffer)
         ;; C-x bindings in `ctl-x-map'
         ;; ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-c b o" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-c b m" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-c b p" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("C-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("s-1" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("s-d" . consult-find)                  ;; Alternative: consult-fd
         ;; ("M-s c" . consult-locate)
         ;; ("M-s L" . consult-line-multi)
         ;; ("M-s k" . consult-keep-lines)
         ;; ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ;; ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ;; ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ;; ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ;; ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ;; ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ;; ("M-r" . consult-history)
         )                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
    ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
    ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
    ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
    ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
    ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :config
  ;; A function that returns a list of directories
  (defun consult-dir--fasd-dirs ()
    "Return list of fasd dirs."
    (split-string (shell-command-to-string "fasd -ld") "\n" t))

  ;; A consult source that calls this function
  (defvar consult-dir--source-fasd
    `(:name     "Fasd dirs"
                :narrow   ?f
                :category file
                :face     consult-file
                :history  file-name-history
                :enabled  ,(lambda () (executable-find "fasd"))
                :items    ,#'consult-dir--fasd-dirs)
    "Fasd directory source for `consult-dir'.")

  ;; Adding to the list of consult-dir sources
  (add-to-list 'consult-dir-sources 'consult-dir--source-fasd t))

;; 自定义的模式集合
(defvar my-web-modes
  '(tsx-ts-mode
    typescript-ts-mode
    json-ts-mode
    js-ts-mode
    prisma-ts-mode
    typescript-mode
    js2-mode
    web-mode
    html-mode
    css-mode
    scss-mode
    go-ts-mode)
  "List of modes for web development.")

;; 通用的钩子启用函数
(defun my-enable-hooks (modes hook-fn)
  "Enable HOOK-FN for MODES."
  (dolist (mode modes)
    (add-hook (intern (concat (symbol-name mode) "-hook")) hook-fn)))

(add-to-list 'auto-mode-alist '("\\.[cm]?js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :diminish smartparens-mode
  :bind
  ("s-(" . sp-backward-sexp)
  ("s-)" . sp-forward-sexp)
  ("C-(" . sp-down-sexp)
  ("C-)" . sp-up-sexp)
  :config
  (sp-use-smartparens-bindings))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :bind (("C-c y i" . yas-insert-snippet)
         ("C-c y f" . yas-visit-snippet-file)
         ("C-c y n" . yas-new-snippet)
         ;; ("C-c y t" . yas-tryout-snippet)
         ;; ("C-c y l" . yas-describe-tables)
         ;; ("C-c y g" . yas-global-mode)
         ;; ("C-c y m" . yas-minor-mode)
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

(use-package nginx-mode
  :mode
  "/nginx/.+\\.conf\\'"
  "nginx\\.conf\\'")

(use-package markdown-mode
  :mode
  "\\.markdown\\'"
  "\\.md\\'"
  :hook
  (markdown-mode-hook . markdown-display-inline-images)
  :init
  (setq markdown-enable-wiki-links t)
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-header-scaling t)
  (setq markdown-hide-markup t)
  (setq markdown-italic-underscore t)
  (setq markdown-blockquote-display-char '("┃" ">"))
  (setq markdown-list-item-bullets '("⏺" "▪"))
  (setq markdown-make-gfm-checkboxes-buttons t)
  (setq markdown-max-image-size '(1024 . 1024)))

(use-package js-doc
  :config
  (setq js-doc-mail-address user-mail-address
        js-doc-author (format "<%s> <%s>" user-full-name js-doc-mail-address)
        ;; js-doc-url user-blog-url
        ;; js-doc-license "MIT"
        ))

(use-package web-mode
  :mode ("\\.html?\\'" "\\.vue\\'")
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

(use-package js2-mode
  :mode "\\.jsx?\\'"
  :config
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)

  ;; Set up proper indentation in JavaScript and JSON files
  (setq-default js-indent-level 2))

(use-package typescript-ts-mode
  :mode "\\.[cm]?tsx?\\'"
  :config
  (setq typescript-indent-level 2))

(use-package scss-mode)
(use-package css-mode)

(use-package emmet-mode
  :diminish emmet-mode
  :hook ((sgml-mode html-mode css-mode web-mode typescript-mode js-mode) . emmet-mode)
  :config
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))))

(use-package json-mode
  :defer t
  :mode ("\\.json$" . json-mode))

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

(use-package apheleia
  :defer t
  :diminish apheleia-mode
  ;; :hook
  ;; (prog-mode-hook . apheleia-mode)
  ;; :config
  ;; (dolist (formatter '((eslint . (npx "eslint_d" "--fix-to-stdout" "--stdin" "--stdin-filename" file))
  ;;                      (nix . ("nix" "fmt" "--" "-"))
  ;;                      (rufo . ("rufo" "--simple-exit"))))
  ;;   (cl-pushnew formatter apheleia-formatters :test #'equal))
  )

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

(use-package kind-icon
  :after corfu
  :custom (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :bind (("s-'" . vertico-repeat))
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  :config
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  )

(use-package vertico-directory
  :straight vertico
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package savehist
  :init
  (savehist-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (
   ("C-." . embark-act)         ;; pick some comfortable binding
   ;; ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package lsp-bridge
  :diminish (lsp-bridge-mode . "℗ ")
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
                         :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                         :build (:not compile)
                         )
  :bind (
         ;; ("C-c l d" . lsp-bridge-popup-documentation)
         ("C-c l r" . lsp-bridge-restart-process)
         ("C-c l R" . lsp-bridge-rename)
         ("C-c l j" . lsp-bridge-peek-jump)
         ("C-c l b" . lsp-bridge-peek-jump-back)
         ("C-c e l" . lsp-bridge-diagnostic-list)
         ("C-c e n" . lsp-bridge-diagnostic-jump-next)
         ("C-c e p" . lsp-bridge-diagnostic-jump-prev)
         ("C-7" . lsp-bridge-find-def)
         ("C-8" . lsp-bridge-find-def-return)
         ("C-S-7" . lsp-bridge-workspace-list-symbol-at-point)
         ("C-S-8" . lsp-bridge-workspace-list-symbols)
         ("C-9" . lsp-bridge-find-references)
         )
  :config
  (setq lsp-bridge-python-command "/usr/bin/python3")
  (require 'lsp-bridge)
  (require 'lsp-bridge-jdtls)

  (setq lsp-bridge-enable-completion-in-minibuffer t)
  (setq lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame)
  (setq lsp-bridge-enable-with-tramp t)
  (setq lsp-bridge-enable-org-babel t)
  (setq acm-enable-capf t)
  (setq acm-enable-quick-access t)
  (setq acm-backend-yas-match-by-trigger-keyword t)
  (setq acm-enable-tabnine nil)
  (setq acm-enable-codeium nil)

  (global-lsp-bridge-mode)

  (add-to-list 'lsp-bridge-multi-lang-server-extension-list '(("html") . "html_tailwindcss"))

  (add-to-list 'lsp-bridge-multi-lang-server-extension-list '(("vue") . "html_tailwindcss"))
  (add-to-list 'lsp-bridge-multi-lang-server-extension-list '(("css") . "css_tailwindcss"))

  (setq lsp-bridge-csharp-lsp-server "csharp-ls")
  (defun my/bridge-server-setup ()
    (with-current-buffer (current-buffer)
      (when (bound-and-true-p acm-backend-lsp-server-names)
        (let ((servers acm-backend-lsp-server-names))
          ;; enable : in emmet completion
          (when (member "emmet-ls" servers)
            (setq-local lsp-bridge-completion-hide-characters
                        (delete ":" lsp-bridge-completion-hide-characters)))
          ;; enable - in tailwindcss completion
          (when (member "tailwindcss" servers)
            (modify-syntax-entry ?- "w"))))))

  (add-hook 'lsp-bridge-mode-hook
            (lambda ()
              (run-with-timer 3 nil #'my/bridge-server-setup)))
  )

;; 打开日志，开发者才需要
;; (setq lsp-bridge-enable-log t)

(defun my-eslint-fix-file ()
  "Run `npx eslint --fix --cache` on the current file."
  (when (and (buffer-file-name)
             (string-match-p "\\.\\(js\\|jsx\\|ts\\|tsx\\|vue\\|html\\)\\'" (buffer-file-name)))
    (let* ((eslint-command (concat "npx eslint --fix --cache " (shell-quote-argument (buffer-file-name))))
           (output-buffer (generate-new-buffer "*eslint-output*")))
      (message "Running eslint --fix...")
      (if (= 0 (call-process-shell-command eslint-command nil output-buffer))
          (progn
            (message "eslint --fix completed successfully.")
            (kill-buffer output-buffer))
        ;; (progn
          ;; (message "eslint --fix failed. Check *eslint-output* for details.")
          ;; (pop-to-buffer output-buffer))
        ))))

(defun my-add-eslint-fix-hook ()
  "Add the `my-eslint-fix-file` function to the `after-save-hook`."
  (add-hook 'after-save-hook 'my-eslint-fix-file nil 'local))

;; 针对前端相关的 major modes 启用 eslint fix 钩子
(dolist (hook '(js-mode-hook
                js2-mode-hook
                typescript-mode-hook
                typescript-ts-mode-hook
                tsx-ts-mode-hook
                web-mode-hook
                vue-mode-hook))
  (add-hook hook 'my-add-eslint-fix-hook))

(use-package magit
  :bind* (("C-S-g" . magit))
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

(use-package blamer
  :bind (("C-c g c" . blamer-show-posframe-commit-info))
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

(use-package smerge-mode
  :config
  (defhydra smerge/panel ()
  "smerge"
  ("k" (smerge-prev) "prev change" )
  ("j" (smerge-next) "next change")
  ("u" (smerge-keep-upper) "keep upper")
  ("l" (smerge-keep-lower) "keep lower")
  ("q" nil "quit" :exit t))
  :bind ("s-," . smerge/panel/body))

(use-package diff-hl
:hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
       (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode))

(use-package consult-git-log-grep
:bind (("C-c g l" . consult-git-log-grep))
  :custom
  (consult-git-log-grep-open-function #'magit-show-commit))

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

(global-set-key (kbd "s-`") 'vterm-toggle)
(global-set-key (kbd "s-<return>") 'multi-vterm-project)

(define-key org-mode-map (kbd "s-t") 'org-todo)
(bind-keys*
 ("C-`" . execute-extended-command)
 ("C-x ="     . indent-region)
 ("M-o" . other-window)
 ;; ("M-9" . hs-hide-block)
 ;; ("M-0" . hs-show-block)
 ("M-9" . hs-hide-all)
 ("M-0" . hs-show-all)
 ("M-," . hs-toggle-hiding)
 ("s-b" . switch-to-buffer)
 ("s-J" . scroll-up-command)
 ("s-K" . scroll-down-command)
 ("s-n" . next-buffer)
 ("s-p" . previous-buffer)
 ("s-f" . find-file)
 )

(which-key-add-key-based-replacements
    "M-s" "search")

(which-key-add-key-based-replacements
   "s-g" "goto")

(which-key-add-key-based-replacements
  "C-c TAB" "persp")

(which-key-add-key-based-replacements
  "C-c c" "copy")

(which-key-add-key-based-replacements
  "C-c e" "errors")

(which-key-add-key-based-replacements
  "C-c i" "insert")

(which-key-add-key-based-replacements
    "C-c l" "lsp")

(which-key-add-key-based-replacements
    "C-c s" "search")

(which-key-add-key-based-replacements
    "C-c s" "search")

(which-key-add-key-based-replacements
    "C-c b" "buffer")

(which-key-add-key-based-replacements
    "C-c y" "yas&dict")

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
