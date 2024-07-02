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

(modalka-define-kbd "0" "C-0")
(modalka-define-kbd "1" "C-1")
(modalka-define-kbd "2" "C-2")
(modalka-define-kbd "3" "C-3")
(modalka-define-kbd "4" "C-4")
(modalka-define-kbd "5" "C-5")
(modalka-define-kbd "6" "C-6")
(modalka-define-kbd "7" "C-7")
(modalka-define-kbd "8" "C-8")
(modalka-define-kbd "9" "C-9")

(modalka-define-kbd "h" "C-b")
(modalka-define-kbd "j" "C-n")
(modalka-define-kbd "k" "C-p")
(modalka-define-kbd "l" "C-f")
(modalka-define-kbd "e" "M-f")
(modalka-define-kbd "b" "M-b")
(modalka-define-kbd "n" "M-n")
(modalka-define-kbd "N" "M-p")
(modalka-define-kbd "{" "M-{")
(modalka-define-kbd "}" "M-}")
(modalka-define-kbd "0" "C-a")
(modalka-define-kbd "$" "C-e")
(modalka-define-kbd "G" "M->")
(modalka-define-kbd "y" "M-w")
(modalka-define-kbd "p" "C-y")
(modalka-define-kbd "P" "M-y")
(modalka-define-kbd "x" "C-d")
(modalka-define-kbd "D" "C-k")
(modalka-define-kbd "z" "C-l")
(modalka-define-kbd "!" "M-&")
(modalka-define-kbd "J" "C-v")
(modalka-define-kbd "K" "M-v")
(modalka-define-kbd "M" "C-u")
(modalka-define-kbd "(" "M-a")
(modalka-define-kbd ")" "M-e")
(modalka-define-kbd "/" "C-s")
(modalka-define-kbd "E" "C-g")
(modalka-define-kbd "d" "C-w")
(modalka-define-kbd "w" "C-x o")
(modalka-define-kbd "W" "M-m W-")
(modalka-define-kbd "B" "M-m B")
(modalka-define-kbd "H" "C-x >")
(modalka-define-kbd "L" "C-x <")
(modalka-define-kbd "Z" "C-x 1")
(modalka-define-kbd "q" "C-x (")
(modalka-define-kbd "Q" "C-x )")
(modalka-define-kbd "." "M-m .")
(modalka-define-kbd "?" "M-m ?")
(modalka-define-kbd "v" "C-SPC")
(modalka-define-kbd "V" "M-m V")
(modalka-define-kbd "=" "M-m =")
(modalka-define-kbd "R" "M-m R")


(modalka-define-kbd "X" "C-x C-x")
(modalka-define-kbd "+" "C-x r m")
(modalka-define-kbd "'" "C-x r b")
(modalka-define-kbd "\\" "C-c C-c")

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
(modalka-define-kbd "^" "M-m ^")
(modalka-define-kbd "&" "M-m &")
(modalka-define-kbd "g S" "C-j")
(modalka-define-kbd "g ?" "C-h k")

(modalka-define-kbd "i a" "C-x h")

(modalka-define-kbd ": q" "C-x C-c")
(modalka-define-kbd ": r" "C-x M-c")
(modalka-define-kbd ": t" "M-m : t")

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

(defun exit-on-space ()
  (interactive)
  (modalka-mode 0)
  (insert-char 32))

(defun exit-modalka ()
  (interactive)
  (modalka-mode 0))

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

(modalka-define-kbd "0" "C-0")
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

(modalka-define-kbd "h" "C-b")
(modalka-define-kbd "j" "C-n")
(modalka-define-kbd "k" "C-p")
(modalka-define-kbd "l" "C-f")
(modalka-define-kbd "e" "M-f")
(modalka-define-kbd "b" "M-b")
(modalka-define-kbd "n" "M-n")
(modalka-define-kbd "N" "M-p")
(modalka-define-kbd "{" "M-{")
(modalka-define-kbd "}" "M-}")
(modalka-define-kbd "0" "C-a")
(modalka-define-kbd "$" "C-e")
(modalka-define-kbd "G" "M->")
(modalka-define-kbd "y" "M-w")
(modalka-define-kbd "p" "C-y")
(modalka-define-kbd "P" "M-y")
(modalka-define-kbd "x" "C-d")
(modalka-define-kbd "D" "C-k")
(modalka-define-kbd "z" "C-l")
(modalka-define-kbd "!" "M-&")
(modalka-define-kbd "J" "C-v")
(modalka-define-kbd "K" "M-v")
(modalka-define-kbd "M" "C-u")
(modalka-define-kbd "(" "M-a")
(modalka-define-kbd ")" "M-e")
(modalka-define-kbd "/" "C-s")
(modalka-define-kbd "E" "C-g")
(modalka-define-kbd "d" "C-w")
(modalka-define-kbd "w" "C-x o")
(modalka-define-kbd "W" "M-m W")
(modalka-define-kbd "B" "M-m B")
(modalka-define-kbd "H" "C-x >")
(modalka-define-kbd "L" "C-x <")
(modalka-define-kbd "Z" "C-x 1")
(modalka-define-kbd "q" "C-x (")
(modalka-define-kbd "Q" "C-x )")
(modalka-define-kbd "." "M-m .")
(modalka-define-kbd "?" "M-m ?")
(modalka-define-kbd "v" "C-SPC")
(modalka-define-kbd "V" "M-m V")
(modalka-define-kbd "=" "M-m =")
(modalka-define-kbd "R" "M-m R")
(modalka-define-kbd "X" "C-x C-x")
(modalka-define-kbd "+" "C-x r m")
(modalka-define-kbd "'" "C-x r b")
(modalka-define-kbd "\\" "C-c C-c")
(modalka-define-kbd "," "C-x M-r")

;; 说明
(which-key-add-key-based-replacements
  "ESC" "toggle mode"
  "DEL" "smart del"
  "TAB" "smart tab"
  "RET" "smart enter"
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
  "/" "search"
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

(modalka-define-kbd "i a" "C-x h")

(which-key-add-key-based-replacements
  "i"   "expand prefix"
  "i a" "expand entire buffer")

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
(modalka-define-kbd "^" "M-m ^")
(modalka-define-kbd "&" "M-m &")
(modalka-define-kbd "g S" "C-j")
(modalka-define-kbd "g ?" "C-h k")

;; 说明

(which-key-add-key-based-replacements
  "g"   "global prefix"
  "g g" "start of file"
  "g m" "make frame"
  "g M" "delete frame"
  "g n" "select frame by name"
  "g N" "name frame"
  "g j" "next pdf page"
  "g k" "previous pdf page"
  "g f" "file/url at cursor"
  "g F" "enable follow mode"
  "g o" "eval elisp"
  "g O" "eval defun"
  "g w" "vertical split win"
  "g W" "horizontal split win"
  "g S" "split line"
  "g @" "compose mail"
  "g #" "list eww histories"
  "g x" "browse with eww"
  "g :" "browse with external browser"
  "g {" "eww back"
  "g }" "eww forward"
  "g (" "info previous"
  "g )" "info next"
  "^"   "info up"
  "&"   "info goto"
  "g q" "format para"
  "g ?" "find command bound to key")

(modalka-define-kbd "] ]" "C-x n n")
(modalka-define-kbd "] w" "C-x n w")
(modalka-define-kbd "] s" "M-m ] s")

;; 说明
(which-key-add-key-based-replacements
  "]"   "forward nav/edit"
  "] ]" "narrow region"
  "] s" "next spell error")

(which-key-add-key-based-replacements
  "["   "backward nav/edit"
  "[ [" "widen region")

(modalka-define-kbd "g U" "C-c C-k")
(modalka-define-kbd "SPC SPC" "M-x")
(modalka-define-kbd "SPC a" "C-x b")
(modalka-define-kbd "SPC k" "C-x k")
(modalka-define-kbd "SPC g" "M-g g")
(modalka-define-kbd "SPC d" "C-x d")
(modalka-define-kbd "SPC q" "C-x 0")
(modalka-define-kbd "SPC f" "C-x C-f")
(modalka-define-kbd "SPC w" "C-x C-s")
(modalka-define-kbd "SPC c" "M-m SPC c")
(modalka-define-kbd "SPC R" "M-m SPC R")
(modalka-define-kbd "SPC ?" "M-m SPC ?")

;; 说明
(which-key-add-key-based-replacements
  "SPC"   "custom prefix"
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

(use-package discover-my-major
:ensure t
:bind (("C-h C-m" . discover-my-major)
       ("C-h s-m" . discover-my-mode)))

(use-package hydra
:ensure t)

(use-package vertico
  :ensure t
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
 ("M-m g f"   . find-file-at-point)
 ("M-m g u"   . downcase-region)
 ("M-m g U"   . upcase-region)
 ("M-m g C"   . capitalize-region)
 ("M-m g F"   . follow-mode)
 ("M-m R"     . overwrite-mode)
 ("M-m : t"   . emacs-init-time)
 ("M-m g @"   . compose-mail)
 ("M-m SPC ?" . describe-bindings))

(use-package which-key
:ensure t
:defer t
:diminish which-key-mode
:init
(setq which-key-sort-order 'which-key-key-order-alpha)
:bind* (("s-m ?" . which-key-show-top-level))
:config
(which-key-mode)
(which-key-add-key-based-replacements
  "s-m ?" "top level bindings"))

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
