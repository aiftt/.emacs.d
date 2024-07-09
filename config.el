(defvar os/emacs-dir (expand-file-name "~/.emacs.d/")
    "Directory containing working copy of Emacs config repo.")
  (defvar os/emacs-tmp-dir (concat os/emacs-dir "tmp/")
      "Scratch space for stuff...")

(defun os/kill-this-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer nil))
(bind-key "C-x k" #'os/kill-this-buffer)

(setq user-full-name "Lee ZhiCheng"
      user-mail-address "ftt.loves@gmail.com")

;; DEFAULT DIRECTORY
;; This makes `find-file' in a buffer like `*scratch*' default to
;; $HOME instead of `/'
(setq default-directory "~/")

(setq scroll-margin 3)
(setq next-screen-context-lines 5)
(blink-cursor-mode 0)

(add-hook 'prog-mode (lambda () (subword-mode 1)))

		     ;;; CURSOR
(setq-default cursor-type '(bar . 1))

		     ;;; DEBUGGING
(setq-default warning-minimum-level :warning)

;; 关闭启动画面
(setq inhibit-startup-message t)

(recentf-mode 1)

		    ;;; DESKTOP
(desktop-save-mode -1)

		    ;;; DISABLE / ENABLE
(put 'downcase-region 'disabled nil)
(put 'overwrite-mode  'disabled t)
(put 'upcase-region   'disabled nil)

(setq echo-keystrokes 0.1) ;; 设置尽快显示按键序列

;; 默认启用文本模式
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; 显示列号
(column-number-mode 1)

;; 在 minibuffer 里启用自动补全函数和变量
(icomplete-mode 1)


      ;;; (MENU/SCROLLBAR/TOOLBAR)-MODE
(if (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))

    ;;; MOUSE WHEEL
(if (fboundp 'mouse-wheel-mode) (mouse-wheel-mode t))


;; Use directional window movement (S-<left> to move to the left window)
(windmove-default-keybindings)

(size-indication-mode t)		;显示文件大小
(setq delete-by-moving-to-trash t)	;删除文件直接到回收站

(defalias 'yes-or-no-p 'y-or-n-p)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(use-package savehist
  :custom
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-file (expand-file-name "savehist" os/emacs-tmp-dir))
  :config
  (savehist-mode t))

(use-package saveplace
  :custom
  (save-place-file (expand-file-name "saveplace" os/emacs-tmp-dir))
  :config
  (save-place-mode))

(server-start)

;;; AUTO-SAVES AND BACKUPS
(eval-when-compile (defvar os/emacs-tmp-dir))
(defvar os/backup-dir (concat os/emacs-tmp-dir "saves/" )
  "Place to put file backups.")
(setq auto-save-list-file-prefix (concat os/emacs-tmp-dir "auto-save-list/.saves-"))
(setq auto-save-file-name-transforms `((".*" ,os/emacs-tmp-dir t)))
(setq backup-by-copying t)
(setq backup-directory-alist `((".*" . ,os/backup-dir)))
(setq create-lockfiles nil)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; AUTO CREATE DIRECTORIES
;;;; after <http://atomized.org/2008/12/emacs-create-directory-before-saving/>
(defun os/before-save-hook ()
  "My customizations for `before-save-hook'."
  (or (file-exists-p (file-name-directory buffer-file-name))
      (make-directory (file-name-directory buffer-file-name) t)))
(add-hook 'before-save-hook 'os/before-save-hook)

(use-package async :commands (async-start))
(use-package cl-lib)
(use-package dash)
(use-package s)

(use-package eldoc
  :defer t
  :diminish
  :init
  (global-eldoc-mode))

(use-package autorevert
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  :config
  ;; Also auto refresh dired, but be quiet about it
  (global-auto-revert-mode t))

(use-package whole-line-or-region
  :init
  (whole-line-or-region-global-mode 1))

(use-package keycast)

;;; GARBAGE COLLECTION MAGIC HACK
  ;;;; speeds startup?
  (use-package gcmh
    :ensure gcmh
    :demand
    :diminish gcmh-mode
    :functions
    gcmh-mode
    :init
    (gcmh-mode 1))

  
;;;; per https://github.com/emacs-lsp/lsp-mode#performance
(setq read-process-output-max (* 10 1024 1024)) ;; 10mb
(setq gc-cons-threshold 200000000)

(bind-key "M-o"         #'other-window)
(bind-key "M-i"         #'imenu)
(bind-key "C-c l"        #'org-store-link)
(bind-key "C-c a"        #'org-agenda)
(bind-key "C-c c"        #'org-capture)

(use-package evil
  :defines
  evil-want-C-w-delete
  evil-want-keybinding
  evil-want-integration
  evil-want-C-w-in-emacs-state
  evil-disable-insert-state-bindings
  :functions
  evil-global-set-key
  evil-mode
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-w-delete nil)
  (setq evil-want-C-w-in-emacs-state nil)
  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-mode 0))

(add-hook 'org-mode-hook
          (lambda ()
            (keymap-set org-mode-map "M-s j"
                        'consult-org-heading)))

(global-set-key (kbd "<f1>")
                (lambda ()
                  (interactive)
                  (find-file "~/.emacs.d/config.org")))

(global-set-key (kbd "<f5>") 'reload-init-file)

(defun reload-init-file ()
  "重新加载 init.el 文件的函数"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(use-package restart-emacs
  :bind* (("<f2>" . restart-emacs)))

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
:demand
:functions
exec-path-from-shell-initialize
:init
;; FIXME seeing if this does anything... (setq exec-path-from-shell-check-startup-files nil)
(exec-path-from-shell-initialize)
:custom
(exec-path-from-shell-variables
 '(
   "CARGO_HOME"
   "GOPATH"
   "GOROOT"
   "MANPATH"
   "NVM_DIR"
   "PATH"
   "PLENV_ROOT"
   "RUSTUP_HOME"
   "SSH_AGENT_PID"
   "SSH_AUTH_SOCK"
   )))

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

(use-package flycheck
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error)))

;; `M-x combobulate' (default: `C-c o o') to start using Combobulate
(use-package treesit
  :straight (:type built-in)
  :mode (("\\.tsx\\'" . tsx-ts-mode)
	 ("\\.cmake\\'" . cmake-ts-mode)
	 ("\\.go\\'" . go-ts-mode)
	 ("\\.js\\'" . typescript-ts-mode)
	 ("\\.mjs\\'" . typescript-ts-mode)
	 ("\\.mts\\'" . typescript-ts-mode)
	 ("\\.cjs\\'" . typescript-ts-mode)
	 ("\\.ts\\'" . typescript-ts-mode)
	 ("\\.jsx\\'" . tsx-ts-mode)
	 ("\\.json\\'" . json-ts-mode)
	 ("\\.yaml\\'" . yaml-ts-mode)
	 ("\\.yml\\'" . yaml-ts-mode)
	 ("\\.Dockerfile\\'" . dockerfile-ts-mode)
	 ("\\.prisma\\'" . prisma-ts-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.mdx\\'" . markdown-mode))
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
	     '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
	       (bash "https://github.com/tree-sitter/tree-sitter-bash")
	       (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
	       (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
	       (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
	       (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
	       (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
	       (markdown "https://github.com/ikatyang/tree-sitter-markdown")
	       (make "https://github.com/alemuller/tree-sitter-make")
	       (elisp "https://github.com/Wilfred/tree-sitter-elisp")
	       (cmake "https://github.com/uyha/tree-sitter-cmake")
	       (c "https://github.com/tree-sitter/tree-sitter-c")
	       (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	       (toml "https://github.com/tree-sitter/tree-sitter-toml")
	       (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
	       (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
	       (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
	       (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
	(treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
	   '((python-mode . python-ts-mode)
	     (css-mode . css-ts-mode)
	     (typescript-mode . typescript-ts-mode)
	     (js-mode . typescript-ts-mode)
	     (js2-mode . typescript-ts-mode)
	     (c-mode . c-ts-mode)
	     (c++-mode . c++-ts-mode)
	     (c-or-c++-mode . c-or-c++-ts-mode)
	     (bash-mode . bash-ts-mode)
	     (css-mode . css-ts-mode)
	     (json-mode . json-ts-mode)
	     (js-json-mode . json-ts-mode)
	     (sh-mode . bash-ts-mode)
	     (sh-base-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (os/setup-install-grammars)
  ;; Do not forget to customize Combobulate to your liking:
  ;;
  ;;  M-x customize-group RET combobulate RET
  ;;
  (use-package combobulate
    :preface
    ;; You can customize Combobulate's key prefix here.
    ;; Note that you may have to restart Emacs for this to take effect!
    (setq combobulate-key-prefix "C-c o")

    ;; Optional, but recommended.
    ;;
    ;; You can manually enable Combobulate with `M-x
    ;; combobulate-mode'.
    :hook
    ((python-ts-mode . combobulate-mode)
     (js-ts-mode . combobulate-mode)
     (go-mode . go-ts-mode)
     (html-ts-mode . combobulate-mode)
     (css-ts-mode . combobulate-mode)
     (yaml-ts-mode . combobulate-mode)
     (typescript-ts-mode . combobulate-mode)
     (json-ts-mode . combobulate-mode)
     (tsx-ts-mode . combobulate-mode))
    ;; Amend this to the directory where you keep Combobulate's source
    ;; code.
    ;; :load-path ("~/workspace/combobulate") 
    ))

(use-package consult
  :defines
  consult-customize
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
	 ("C-c M-x" . consult-mode-command)
	 ;;("C-c h" . consult-history)
	 ;;("C-c k" . consult-kmacro)
	 ;;("C-c m" . consult-man)
	 ;;("C-c i" . consult-info)
	 ([remap Info-search] . consult-info)
	 ;; C-x bindings in `ctl-x-map'
	 ;; ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
	 ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	 ;; ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ;; ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
	 ;; ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
	 ;; ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
	 ;; Custom M-# bindings for fast register access
	 ;; ("M-#" . consult-register-load)
	 ;; ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
	 ;; ("C-M-#" . consult-register)
	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop)                ;; orig. yank-pop
	 ;; M-g bindings in `goto-map'
	 ("M-g e" . consult-compile-error)
	 ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
	 ("M-g g" . consult-goto-line)             ;; orig. goto-line
	 ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	 ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)
	 ;; M-s bindings in `search-map'
	 ("M-s d" . consult-find)                  ;; Alternative: consult-fd
	 ("M-s c" . consult-locate)
	 ("M-s g" . consult-grep)
	 ("M-s G" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)
	 ;; Isearch integration
	 ("M-s e" . consult-isearch-history)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
	 ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
	 ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
	 ;; Minibuffer history
	 :map minibuffer-local-map
	 ("M-s" . consult-history)                 ;; orig. next-matching-history-element
	 ("M-r" . consult-history))                ;; orig. previous-matching-history-element

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
	 ("C-c y t" . yas-tryout-snippet)
	 ("C-c y l" . yas-describe-tables)
	 ("C-c y g" . yas-global-mode)
	 ("C-c y m" . yas-minor-mode)
	 ("C-c y r" . yas-reload-all)
	 ("C-c y x" . yas-expand)
	 :map yas-keymap
	 ("C-i" . yas-next-field-or-maybe-expand))
  :config
  (yas-reload-all)
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :defer t
  :after yasnippet)

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)           ; Minimum length of prefix for completion
  (corfu-auto-delay 0)            ; No delay for completion
  (corfu-popupinfo-delay '(0.5 . 0.2))  ; Automatically update info popup after that numver of seconds
  (corfu-preview-current 'insert) ; insert previewed candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . corfu-insert))

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode) ; Popup completion info
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                                   corfu-quit-no-match t
                                   corfu-auto nil)
              (corfu-mode))
            nil
            t))

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
