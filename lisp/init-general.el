;;; -*- lexical-binding: t; -*-

;; --- which-key
(use-package which-key
  :hook (after-init . which-key-mode)
  :ensure t
  :init
  (setq which-key-side-window-location 'bottom)
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 0.1)
  ;;(setq which-key-idle-secondary-delay 0.05)
  (which-key-mode)
  )

;; --- general.el
(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (general-create-definer space-leader-def
    :states '(normal visual)
    :prefix "SPC")

  (general-create-definer comma-leader-def
    :states '(normal visual)
    :prefix ","
    :keymaps 'override)
  )

;; - bindings

(comma-leader-def
  "," 'hydra-everything/body
  "g" 'hydra-smerge/body
  "s" 'hydra-color-rg/body
  )

(space-leader-def
  "TAB" 'projectile-persp-switch-project
  "SPC" 'execute-extended-command
  "," 'delete-window
  "." 'kill-this-buffer
  ";" 'kill-other-window-buffer
  "`" 'multi-vterm-project
  "x" 'scratch-buffer

  ;; - apps
  "a" '(:ignore t :which-key "apps")
  "aa" 'org-agenda

  ;; - buffer
  "b" '(:ignore t :which-key "buffers")
  "bb" 'consult-buffer
  "bd" 'kill-current-buffer
  "bk" 'kill-buffer
  "bn" 'next-buffer
  "bo" 'consult-buffer-other-window
  "bp" 'previous-buffer
  "bf" 'gcl/open-current-directory

  "dl" 'devdocs-lookup
  "dr" 'devdocs-peruse

  ;; - files
  "f" '(:ignore t :which-key "files")
  "ff" '(find-file :which-key "find file")
  "fj" 'direx:jump-to-directory
  "fd" 'crux-delete-buffer-and-file
  "fo" 'crux-open-with

  ;; - open http://baidu.com
  "o" '(:ignore t :which-key "open")
  "om" 'play-sound-file
  "ol" 'link-hint-open-link
  "oc" 'link-hint-copy-link

  ;; - projectile
  "p" '(:ignore t :which-key "projectile")
  "pp" 'consult-projectile-switch-project
  "pf" 'consult-projectile-find-file
  "pd" 'consult-projectile-find-dir
  "pb" 'consult-projectile-switch-to-buffer
  "pl" 'consult-project-buffer

  ;; - search
  "s" '(:ignore t :which-key "search")
  "sf" 'projectile-find-file-in-directory
  "sg" 'rgrep
  "sp" 'consult-ripgrep
  "sr" 'rg
  ;; color-rg
  "si" 'color-rg-search-input-in-project
  "st" 'color-rg-search-symbol-with-type

  ;; - window
  "w" '(:ignore t :which-key "window")
  "w-" 'split-window-below
  "wv" 'split-window-right
  "wm" 'delete-other-windows
  "wh" 'evil-window-left
  "wl" 'evil-window-right
  "wj" 'evil-window-down
  "wk" 'evil-window-up
  )

(general-define-key
 "<s-backspace>" 'crux-kill-line-backwards
 "<s-left>" 'windmove-left
 "<s-right>" 'windmove-right
 "<s-down>" 'windmove-down
 "<s-up>" 'windmove-up

 "s-," 'bury-buffer
 "s-." 'unbury-buffer
 "s-<" 'watch-other-window-up-line
 "s->" 'watch-other-window-down-line
 "s-`" 'vterm-toggle
 "s-'" 'vertico-repeat
 "s-;" 'evilnc-comment-or-uncomment-lines
 "s-d" 'consult-dir
 "s-e" 'transpose-chars
 "s-F" 'format-all-buffer
 "s-i" 'parrot-rotate-next-word-at-point
 "s-m" 'toggle-input-method
 "s-o" 'toggle-one-window
 "s-r" 're-builder
 "s-w" 'transpose-words

 "s-n" 'move-text-down
 "s-p" 'move-text-up

 "M-;" 'evilnc-comment-or-uncomment-lines
 "M-'" 'consult-register-store
 "M-#" 'consult-register-load
 "M-d" 'dash-at-point
 "M-i" 'my-string-inflection-cycle-auto
 "M-s" 'symbol-overlay-put
 "M-n" 'symbol-overlay-switch-forward
 "M-p" 'symbol-overlay-switch-backward
 "M-c" 'symbol-overlay-remove-all
 "M-m" 'blamer-show-posframe-commit-info
 "M-o" 'ace-window

 "C-'" 'toggle-quotes-plus
 "C-k" 'crux-smart-kill-line
 "C-j" 'emmet-expand-yas
 "C-s" 'consult-line

 "C-c SPC" 'just-one-space
 "C-c =" 'math-at-point
 "C-c h" 'consult-history
 "C-c o" 'consult-outline
 "C-c r" 'vr/replace
 "C-c q" 'vr/query-replace
 "C-c m" 'vr/mc-mark
 "C-c u" 'uuidgen

 ;; insert
 "C-c iu" 'org-mac-link-get-link
 "C-c is" 'yas-insert-snippet
 "C-c iy" 'consult-yasnippet
 "C-c ie" 'emojify-insert-emoji
 ;; 15:32:24
 "C-c it" 'gcl/insert-current-time
 ;; 2023-08-11 15:32:21
 "C-c id" 'gcl/insert-standard-date
 ;; 2023/08/11
 "C-c il" 'gcl/insert-changelog-date
 "C-c if" 'js-doc-insert-function-doc
 "C-c iF" 'js-doc-insert-file-doc

 ;; bookmark
 "C-c bb" 'consult-bookmark
 "C-c bs" 'bookmark-set
 "C-c cf" 'gcl/copy-file-name-only
 "C-c cp" 'gcl/copy-file-full-name

 ;; yas & fanyi
 "C-c yy" 'fanyi-dwim2
 "C-c yn" 'yas-new-snippet
 "C-c yr" 'yas-reload-all
 "C-c yv" 'yas-visit-snippet-file
 "C-c ys" 'sdcv-search-pointer+
 "C-c yi" 'sdcv-search-input

 "C-c C-'" 'separedit/edit-swagger-commentary
 ;; "C-x C-f" 'devdocs-lookup
 ;; "C-x C-d" 'devdocs-peruse

 "C-x C-j" 'direx:jump-to-directory
 "C-x j" 'consult-bookmark

 "C-S-n" 'duplicate-line-or-region-above
 "C-S-o" 'duplicate-line-or-region-below
 "C-S-h" 'buf-move-left
 "C-S-l" 'buf-move-right
 "C-s-j" 'buf-move-down
 "C-S-k" 'buf-move-up
 )

;; 指定模式的按键
(general-define-key
 :keymaps '(evil-normal-state-map evil-motion-state-map evil-insert-state-map)
 "C-r" 'crux-rename-buffer-and-file
 "C-w" 'evil-delete-backward-word
 "C-p" 'previous-line
 "C-n" 'next-line

 "C-a" 'crux-move-beginning-of-line
 )


(provide 'init-general)
