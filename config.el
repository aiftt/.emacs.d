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

(modalka-define-kbd ": q" "C-x C-c")
(modalka-define-kbd ": r" "C-x M-c")
(modalka-define-kbd ": t" "M-m : t")

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

(use-package orderless
  :ensure t
  :after vertico
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
  :ensure t
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
  :ensure t
  :after (embark consult))

(use-package marginalia
  :ensure t
  :after vertico
  :demand t                     ; :demand applies to :bind but not
					; :after.  We want to eagerly load
					; marginalia once vertico is loaded.
  :bind (:map minibuffer-local-map
	      ("C-o" . marginalia-cycle))
  :config (marginalia-mode 1))

(use-package consult
  :ensure t
  :bind (:map consult-mode-map
         ;; M-s …
         ("M-s u" . consult-focus-lines)
         ("M-s k" . consult-keep-lines)
         ("M-s e" . consult-isearch-history)
         ("M-s d" . consult-find)
         ;; M-g …
         ("M-g g" . consult-line)
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-info)
         ("M-g r" . consult-ripgrep)
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
