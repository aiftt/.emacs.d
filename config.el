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
  :defer t
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
              ("M-P" . org-move-subtree-up))
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

(use-package perspective
  :bind
  ;; ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
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
         ;; C-x bindings in `ctl-x-map'
         ;; ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-c b o" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ;; ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-c b p" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("s-1" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("s-d" . consult-find)                  ;; Alternative: consult-fd
         ;; ("M-s c" . consult-locate)
         ;; ("M-s g" . consult-grep)
         ;; ("M-s G" . consult-git-grep)
         ;; ("M-s r" . consult-ripgrep)
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

(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match 'separator)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  :init
  (global-corfu-mode))

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
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

(define-key org-mode-map (kbd "s-t") 'org-todo)
(bind-keys*
 ("C-x ="     . indent-region)
 ("s-o" . other-window))

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
