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
:bind* (("C-x M-c" . restart-emacs)))

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
