;;; -*- lexical-binding: t; -*-

;; --- fanyi
;; --- fanyi
(use-package fanyi
  :ensure t
  :config
  (custom-set-variables
   '(fanyi-providers '(fanyi-haici-provider
			                 fanyi-youdao-thesaurus-provider
			                 fanyi-etymon-provider
			                 fanyi-longman-provider
			                 ;; fanyi-libre-provider
			                 )))
  ;; 还要自动选择翻译内容 buffer
  (setq fanyi-auto-select nil))


(use-package youdao-dictionary
  :ensure t)

(use-package sdcv
  :ensure t
  :config
  ;; say word after translation
  (setq sdcv-say-word-p t)
  ;; setup directory of stardict dictionary
  (setq sdcv-dictionary-data-dir (concat user-emacs-directory "sdcv-dict"))
  (setq sdcv-dictionary-simple-list    ;setup dictionary list for simple search
        '("懒虫简明英汉词典"
          "懒虫简明汉英词典"
          "KDic11万英汉词典"))
  (setq sdcv-dictionary-complete-list     ;setup dictionary list for complete search
        '(
          "懒虫简明英汉词典"
          "英汉汉英专业词典"
          "XDICT英汉辞典"
          "stardict1.3英汉辞典"
          "WordNet"
          "XDICT汉英辞典"
          "Jargon"
          "懒虫简明汉英词典"
          "FOLDOC"
          "新世纪英汉科技大词典"
          "KDic11万英汉词典"
          "朗道汉英字典5.0"
          "CDICT5英汉辞典"
          "新世纪汉英科技大词典"
          "牛津英汉双解美化版"
          "21世纪双语科技词典"
          "quick_eng-zh_CN"
          ))
  )

(provide 'init-fanyi)
