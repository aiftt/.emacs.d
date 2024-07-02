(setq user-full-name "Lee Zhicheng")

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

;; initial window
(setq initial-frame-alist
      '((width . 102)   ; characters in a line
	(height . 54))) ; number of lines

;; sebsequent frame
(setq default-frame-alist
      '((width . 100)   ; characters in a line
	(height . 52))) ; number of lines

;; Bar cursor
(setq-default cursor-type '(bar . 1))
;; 光标不闪烁
(blink-cursor-mode -1)

(setq inhibit-startup-message t
    initial-scratch-message ""
    initial-major-mode 'fundamental-mode
    inhibit-splash-screen t)

;;  (when (eq system-type 'darwin)
;;  (setq mac-option-modifier 'meta))

;; 统一备份到临时文件目录 /tmp/.saves
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "/tmp/.saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
    create-lockfiles nil)

(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-auto-unix)

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(setq-default truncate-lines t)

(setq large-file-warning-threshold (* 15 1024 1024))

(fset 'yes-or-no-p 'y-or-n-p)

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

(setq ediff-window-setup-function 'ediff-setup-windows-plain
    ediff-split-window-function 'split-window-horizontally)

(setq tramp-default-method "ssh"
    tramp-backup-directory-alist backup-directory-alist
    tramp-ssh-controlmaster-options "ssh")

(subword-mode)

(setq sentence-end-double-space nil)

(setq search-whitespace-regexp ".*?")

(savehist-mode)

(put 'narrow-to-region 'disabled nil)

(setq doc-view-continuous t)

(when (fboundp 'winner-mode)
(winner-mode 1))

(setq recentf-max-saved-items 1000
      recentf-exclude '("/tmp/" "/ssh:"))
(recentf-mode)

(setq ns-use-native-fullscreen nil)

(use-package async
  :ensure t
  :commands (async-start))
  
(use-package cl-lib
  :ensure t)

(use-package dash
  :ensure t)

(use-package s
  :ensure t)

(use-package which-key
:ensure t
:defer t
:diminish which-key-mode
:init
(setq which-key-sort-order 'which-key-key-order-alpha)
:bind* (("M-m ?" . which-key-show-top-level))
:config
(which-key-mode)
(which-key-add-key-based-replacements
  "M-m ?" "top level bindings"))

(use-package restart-emacs
:ensure t
:bind* (("<f2>" . restart-emacs)))

(use-package modalka
:ensure t
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
(add-to-list 'modalka-excluded-modes 'eshell-mode)
(add-to-list 'modalka-excluded-modes 'deft-mode)
(add-to-list 'modalka-excluded-modes 'term-mode)
(which-key-add-key-based-replacements
  "M-m"     "Modalka prefix"
  "M-m :"   "extended prefix"
  "M-m m"   "move prefix"
  "M-m s"   "send code prefix"
  "M-m SPC" "user prefix"
  "M-m g"   "global prefix"
  "M-m o"   "org prefix"
  "M-m a"   "expand around prefix"
  "M-m i"   "expand inside prefix"
  "M-m ["   "prev nav prefix"
  "M-m ]"   "next nav prefix"))

(use-package flyspell
:diminish (flyspell-mode . "φ")
:bind* (("M-m ] s" . flyspell-goto-next-error)))

(use-package dired
  :bind (:map dired-mode-map
	      ("C-c C-e" . wdired-change-to-wdired-mode))
  :init
  (setq dired-dwim-target t
	dired-recursive-copies 'top
	dired-recursive-deletes 'top
	dired-listing-switches "-alh")
  :config
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

(use-package exec-path-from-shell
:ensure t
:demand t
:init
(setq exec-path-from-shell-check-startup-files nil)
:config
;; (exec-path-from-shell-copy-env "PYTHONPATH")
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

(use-package diminish
:ensure t
:demand t
:diminish (visual-line-mode . "ω")
:diminish hs-minor-mode
:diminish abbrev-mode
:diminish auto-fill-function
:diminish subword-mode
:diminish eldoc-mode
)

(defun sk/diminish-org-indent ()
  (interactive)
  (diminish 'org-indent-mode ""))
(add-hook 'org-indent-mode-hook 'sk/diminish-org-indent)

(defun sk/diminish-auto-revert ()
  (interactive)
  (diminish 'auto-revert-mode ""))
(add-hook 'auto-revert-mode-hook 'sk/diminish-auto-revert)

(bind-keys*
 ("C-r"       . dabbrev-expand)
 ("s-/"       . hippie-expand)
 ("C-S-d"     . kill-whole-line)
 ("s-m SPC c" . load-theme)
 ("s-m SPC r" . locate)
 ("s-m w"     . winner-undo)
 ("s-m g m"   . make-frame)
 ("s-m g M"   . delete-frame)
 ("s-m g n"   . select-frame-by-name)
 ("s-m g n"   . set-frame-name)
 ("s-m b"     . mode-line-other-buffer)
 ("s-m ="     . indent-region)
 ("s-m g f"   . find-file-at-point)
 ("s-m g u"   . downcase-region)
 ("s-m g U"   . upcase-region)
 ("s-m g C"   . capitalize-region)
 ("s-m g F"   . follow-mode)
 ("s-m R"     . overwrite-mode)
 ("s-m : t"   . emacs-init-time)
 ("s-m g @"   . compose-mail)
 ("s-m SPC ?" . describe-bindings))

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
