;;; -*- lexical-binding: t; -*-


;; --- window
(use-package toggle-one-window
  :init (slot/vc-install :repo "manateelazycat/toggle-one-window"))

(use-package watch-other-window)

(use-package ace-window :ensure t)

(use-package popup :ensure t)

(setq display-buffer-base-action
      '(
        ;; 当新的 buffer 与当前 window 显示的 buffer 具有相同的 major
        ;; mode 时，重用当前 window 显示该 buffer
        display-buffer-reuse-mode-window
        ;; 尝试重用当前 window 显示新的 buffer，不管 major mode 是否相同
        display-buffer-reuse-window
        ;; 尝试在当前 window 显示新的 buffer，如果没有其他窗口可用
        display-buffer-same-window)
      ;; 默认情况下，Emacs 会尽量保持窗口大小均匀分布，但这会导致某些
      ;; 情况下窗口大小不符合期望，因此通过设置 even-window-sizes 为
      ;; nil 可以禁用这个自动平衡功能
      even-window-sizes nil)


(provide 'init-window)
