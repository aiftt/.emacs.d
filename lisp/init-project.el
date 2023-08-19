;;; -*- lexical-binding: t; -*-

;; --- persp
(use-package perspective
  :ensure t
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c TAB"))  ; pick your own prefix key here
  :init
  (persp-mode)
  :config
  (setq persp-state-default-file (expand-file-name ".cache/gcl" user-emacs-directory))
  (setq persp-show-modestring 'header)
  (add-hook 'kill-emacs-hook #'persp-state-save)
  (use-package persp-projectile
    :ensure t)
  )

(with-eval-after-load 'general
  (general-define-key
   "s-1" '(lambda () (interactive) (persp-switch-by-number 1))
   "s-2" '(lambda () (interactive) (persp-switch-by-number 2))
   "s-3" '(lambda () (interactive) (persp-switch-by-number 3))
   "s-4" '(lambda () (interactive) (persp-switch-by-number 4))
   "s-5" '(lambda () (interactive) (persp-switch-by-number 5))
   "s-)" 'persp-next
   "s-(" 'persp-prev
   ))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;; alien, hybrid
  (setq projectile-indexing-method 'alien projectile-enable-caching t)
  )


(provide 'init-project)
