;;; -*- lexical-binding: t; -*-

;; --- search
(use-package devdocs
  :ensure t)

(use-package engine-mode
  :ensure t
  :config
  (engine-mode t)
  (engine/set-keymap-prefix (kbd "C-c s"))
  (defengine baidu "https://www.baidu.com/s?wd=%s"
	           :keybinding "b")
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")
  (defengine qwant
    "https://www.qwant.com/?q=%s"
    :docstring "什么都能搜到哦~~😍😍"
    :keybinding "q")
  (defengine rfcs
    "http://pretty-rfc.herokuapp.com/search?q=%s"
    :keybinding "r")
  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")
  (defengine twitter
    "https://twitter.com/search?q=%s"
    :keybinding "t")
  (defengine wolfram-alpha
    "http://www.wolframalpha.com/input/?i=%s"
    :docstring "数学搜索引擎，公式，坐标图等。"
    :keybinding "w")
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "/")
  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y")
  )

(use-package wgrep
  :ensure t)
(setq wgrep-auto-save-buffer t)

(use-package rg
  :ensure t)

(use-package visual-regexp
  :ensure t)

(use-package visual-regexp-steroids
  :ensure t)

(use-package color-rg
  :init (slot/vc-install :repo "manateelazycat/color-rg"))


(provide 'init-search)
