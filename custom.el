(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fanyi-providers
   '(fanyi-haici-provider fanyi-youdao-thesaurus-provider fanyi-etymon-provider fanyi-longman-provider))
 '(org-safe-remote-resources
   '("\\`https://aiftt\\.github\\.io/org-html-themes/org/theme-readtheorg-local\\.setup\\'" "\\`https://aiftt\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'" "\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'"))
 '(package-selected-packages
   '(fringe-helper origami emacs-smart-hungry-delete smart-hungry-delete sis modus-vivendi-theme ace-pinyin sunrise filetree direx smartparens emojify highlight-thing ace-window restclient watch-other-window toggle-one-window window-numbering winum js-doc web-mode emmet-mode scss-mode json-mode typescript-mode js2-mode vterm-toggle multi-vterm vterm dockerfile-mode httprepl devdocs dash-at-point uuidgen math-at-point link-hint crux nvm separedit color-rg visual-regexp-steroids visual-regexp rg wgrep engine-mode prisma-mode orderless flycheck format-all rust-mode lua-mode sql-indent php-mode nxml-mode python-mode yaml-imenu yaml-mode persp-projectile perspective parrot org-roam-ui org-special-block-extras org-download org-fragtog org-mac-link org-superstar org-appear evil-org org-tempo svg-tag-mode -t org-super-agenda go-mode git-modes blamer magit string-inflection hydra emacs-maple-iedit maple-iedit iedit duplicate-line toggle-quotes-plus editorconfig move-text symbol-overlay hungry-delete expand-region sdcv youdao-dictionary fanyi embark-consult embark vertico all-the-icons-completion leuven-theme awesome-tray rainbow-mode rainbow-delimiters highlight-parentheses posframe all-the-icons-dired all-the-icons marginalia consult-project-extra consult-yasnippet consult-projectile consult-org-roam consult-notes consult-ls-git consult-dir consult dumb-jump lsp-bridge lsp-ui lsp-clients lsp-mode markdown-mode yasnippet-snippets general which-key auto-save evil-surround evil-nerd-commenter evil exec-path-from-shell))
 '(package-vc-selected-packages
   '((origami :vc-backend Git :url "https://www.github.com/elp-revive/origami.el")
     (emacs-smart-hungry-delete :vc-backend Git :url "https://www.github.com/hrehfeld/emacs-smart-hungry-delete")
     (toggle-one-window :vc-backend Git :url "https://www.github.com/manateelazycat/toggle-one-window")
     (math-at-point :vc-backend Git :url "https://www.github.com/shankar2k/math-at-point")
     (color-rg :vc-backend Git :url "https://www.github.com/manateelazycat/color-rg")
     (emacs-maple-iedit :vc-backend Git :url "https://www.github.com/honmaple/emacs-maple-iedit")
     (toggle-quotes-plus :vc-backend Git :url "https://www.github.com/jcs-elpa/toggle-quotes-plus")
     (awesome-tray :vc-backend Git :url "https://www.github.com/manateelazycat/awesome-tray")
     (lsp-bridge :vc-backend Git :url "https://www.github.com/manateelazycat/lsp-bridge")
     (auto-save :vc-backend Git :url "https://www.github.com/manateelazycat/auto-save")))
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
           (lambda nil
             (async-start
              '(lambda nil
                 (gcl/org-export-on-save)
                 (message "Export on save completed."))
              nil))
           nil t)
     (eval add-hook 'after-save-hook 'gcl/org-export-on-save nil t)
     (prettify-symbols-alist
      ("lambda" . 955))
     (eval imenu-add-menubar-index)
     (delete-trailing-lines . t)
     (eval let
           ((case-fold-search t))
           (highlight-phrase "[.:~/]*\\(?:shynur\\|谢骐\\)\\(?:[.:/-]+[[:alnum:].:/*-]*\\)?" 'underline))
     (eval when-let
           ((buffer-file-name
             (buffer-file-name)))
           (when
               (string-match-p "\\`\\(?:LICENSE\\|COPYING\\)\\(?:\\.[^.[blank]]+\\)?\\'"
                               (file-name-nondirectory buffer-file-name))
             (setq-local buffer-read-only t)))
     (which-func-modes . t)
     (eval ignore-error 'imenu-unavailable
           (imenu-add-menubar-index))
     (imenu-auto-rescan . t)
     (outline-minor-mode-prefix .
                                [nil]))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t :background "chartreuse")) nil "该face仅有‘:background’字段有效")
 '(fill-column-indicator ((t :background "black" :foreground "yellow")))
 '(font-lock-comment-face ((t (:foreground "#73797e"))))
 '(indent-guide-face ((t :foreground "dark sea green")))
 '(line-number ((t :slant italic :weight light)))
 '(line-number-current-line ((t :slant normal :weight black)))
 '(line-number-major-tick ((t :foreground unspecified :background unspecified :slant italic :underline t :weight light)) nil "指定倍数的行号;除此以外,还有‘line-number-minor-tick’实现相同的功能,但其优先级更低")
 '(tooltip ((t :height 100 :background "dark slate gray")))
 '(window-divider ((t :foreground "SlateBlue4"))))
