;;; -*- lexical-binding: t; -*-


;; --- magit
(use-package magit
  :ensure t
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

(use-package diff-hl
  :ensure
  :config
  (global-diff-hl-mode))

(provide 'init-magit)
