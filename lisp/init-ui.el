;;; -*- lexical-binding: t; -*-

;; --- ui 界面管理
(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :ensure t
  :hook ((dired-mode . all-the-icons-dired-mode)))
(use-package posframe
  :ensure t)

(use-package highlight-parentheses
  :ensure t
  :hook (prog-mode . highlight-parentheses-mode)
  :config
  (add-hook 'minibuffer-setup-hook #'highlight-parentheses-minibuffer-setup)
  )

(use-package emojify
  :disabled t
  :ensure t
  :hook (after-init . global-emojify-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :defer t
  :hook ((prog-mode org-mode) . rainbow-mode))

;; - header line
(set-face-attribute 'header-line nil
                    :foreground (face-attribute 'mode-line :foreground)
                    :background (face-attribute 'mode-line :background)
                    ;; height of mode-line is also unspecified, so we set it directly.
                    :height 150
                    :box (face-attribute 'mode-line :box))

;; - cursor background color
(set-face-attribute 'cursor nil :background "green")
(set-face-attribute 'default nil :font "Fira Code" :height 130)

;; - mode line
(use-package awesome-tray
  :init (slot/vc-install :repo "manateelazycat/awesome-tray")
  :config
  (setq awesome-tray-mode-line-height 0.1
	      awesome-tray-mode-line-active-color "#EC4899"
	      awesome-tray-mode-line-inactive-color "#959eb1"
	      awesome-tray-active-modules '(
				                              ;; "location"
				                              "pdf-view-page"
				                              "date"
				                              "file-path"
				                              "buffer-name"
				                              "mode-name"
				                              "battery"
				                              "git"
				                              "input-method"
				                              "evil"
				                              ;; "flymake"
				                              "belong"
				                              "anzu"
				                              ;; "github"
				                              )
	      awesome-tray-date-format "%d/%H:%M:%S")
  (awesome-tray-mode 1))

(provide 'init-ui)
