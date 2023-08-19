;;; -*- lexical-binding: t; -*-

;; - cursor background color
(set-face-attribute 'cursor nil :background "green")


; 使 frame 根据 背景色的 亮暗, 让 face 自行选择对应的方案.
(setq frame-background-mode nil)

(setq frame-resize-pixelwise t)

;; Scroll 以使 window 底端的 N 行呈现到顶端.
;; (setq next-screen-context-lines 5)


;; - header line
(set-face-attribute 'header-line nil
                    :foreground (face-attribute 'mode-line :foreground)
                    :background (face-attribute 'mode-line :background)
                    ;; height of mode-line is also unspecified, so we set it directly.
                    :height 150
                    :box (face-attribute 'mode-line :box))

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

;; --- ui 界面管理
(provide 'init-ui)
