;;; -*- lexical-binding: t; -*-


;; --- hydra
(use-package hydra :ensure t)
;; --- smerge
(defhydra hydra-smerge (:color red :hint nil)
  "
Navigate       Keep               other
----------------------------------------
_p_: previous  _c_: current       _e_: ediff
_n_: next      _m_: mine  <<      _u_: undo
_j_: up        _o_: other >>      _r_: refine
_k_: down      _a_: combine       _q_: quit
               _b_: base
"
  ("n" smerge-next)
  ("p" smerge-prev)
  ("c" smerge-keep-current)
  ("m" smerge-keep-mine)
  ("o" smerge-keep-other)
  ("b" smerge-keep-base)
  ("a" smerge-keep-all)
  ("e" smerge-ediff)
  ("j" previous-line)
  ("k" forward-line)
  ("r" smerge-refine)
  ("u" undo)
  ("q" nil :exit t))

(defhydra hydra-color-rg (:color green :hint nil)
  "
搜索当前目录          搜索当前项目           搜索当前文件
------------------------------------------------------
_i_: Input          _a_: Input           _x_: Input
_s_: Symbol         _b_: Symbol          _y_: Symbol
_t_: Type           _c_: Type

"
  ;; current dir
  ("i" color-rg-search-input)
  ("s" color-rg-search-symbol)
  ("t" color-rg-search-symbol-with-type)
  ;; current project
  ("a" color-rg-search-input-in-project)
  ("b" color-rg-search-symbol-in-project)
  ("c" color-rg-search-project-with-type)
  ;; current file
  ("x" color-rg-search-input-in-current-file)
  ("y" color-rg-search-symbol-in-current-file)
  ("q" nil :exit t))

(defhydra hydra-everything (:hint nil :columns 3 :exit t)
  "快捷操作
-------------------------------------------------------------------------------------"
  ("1" gcl/open-init-file "打开Emacs配置")
  )


(provide 'init-hydra)
