;;; -*- lexical-binding: t; -*-

;; --- vterm
(use-package vterm :ensure t)
(use-package multi-vterm :ensure t)
(use-package vterm-toggle :ensure t)
(add-hook 'vterm-mode-hook
          (lambda ()
            (setq-local evil-insert-state-cursor 'box)
            (evil-insert-state)))
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


(provide 'init-terminal)
