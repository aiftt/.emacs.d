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
;; -- load path
(add-subdirs-to-load-path "~/.emacs.d/extensions/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

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

(require 'init-func)
(require 'init-basic)
(require 'init-env)
(require 'init-evil)
(require 'init-theme)
(require 'init-ui)
(require 'init-consult)
(require 'init-fanyi)
(require 'init-magit)
(require 'init-project)
(require 'init-web)
(require 'init-mode)
(require 'init-program)
(require 'init-edit)
(require 'init-hydra)
(require 'init-snippet)
(require 'init-lsp)
(require 'init-search)
(require 'init-org)
(require 'init-window)
(require 'init-tools)
(require 'init-terminal)
(require 'init-dired)
(require 'init-general)
(require 'init-chat)

;; --- global 按键设置
(global-set-key (kbd "<f5>") 'gcl/reload-init-file)
(global-set-key (kbd "<f1>") 'gcl/open-init-file)
(global-set-key (kbd "<f2>") 'restart-emacs)
(global-set-key (kbd "<f4>") 'gcl/open-ztd-document)

;; --- 清理
(defun gcl/cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold  (* 1024 200)) ; 200MB
  (setq gc-cons-percentage 0.5) ; 0.5s
  (garbage-collect))
(run-with-idle-timer 4 nil #'gcl/cleanup-gc)
