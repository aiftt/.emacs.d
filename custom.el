(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fanyi-providers
   '(fanyi-haici-provider fanyi-youdao-thesaurus-provider fanyi-etymon-provider fanyi-longman-provider))
 '(org-safe-remote-resources
   '("\\`https://aiftt\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'"))
 '(package-selected-packages
   '(emmet-mode youdao-dictionary yasnippet-snippets yaml-imenu winum window-numbering which-key web-mode vterm-toggle visual-regexp-steroids vertico uuidgen typescript-mode toggle-quotes-plus toggle-one-window symbol-overlay svg-tag-mode string-inflection sql-indent smartparens sis separedit sdcv scss-mode rust-mode rg restclient rainbow-mode rainbow-delimiters python-mode php-mode persp-projectile parrot org-superstar org-super-agenda org-special-block-extras org-roam-ui org-mac-link org-fragtog org-download org-appear org-ai orderless nvm multi-vterm move-text math-at-point marginalia magit lua-mode lsp-ui llm link-hint leuven-theme json-mode js2-mode js-doc iedit hydra hungry-delete httprepl htmlize highlight-thing highlight-parentheses gptel go-mode git-modes general format-all flycheck filetree fanyi expand-region exec-path-from-shell evil-surround evil-org evil-nerd-commenter engine-mode emojify emmet2-mode embark-consult emacs-smart-hungry-delete emacs-maple-iedit ellama editorconfig dumb-jump doom-themes dockerfile-mode dirvish direx diff-hl devdocs deno-bridge dash-at-point crux consult-yasnippet consult-projectile consult-project-extra consult-org-roam consult-notes consult-ls-git consult-dir color-rg blamer awesome-tray auto-save all-the-icons-dired all-the-icons-completion ace-window ace-pinyin))
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook 'my/org-export-on-save nil t)
     (eval add-hook 'after-save-hook
           (lambda nil
             (async-start
              '(lambda nil
                 (gcl/org-export-on-save)
                 (message "Export on save completed."))
              nil))
           nil t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "#73797e"))))
 '(line-number ((t (:foreground "#888888"))))
 '(line-number-current-line ((t (:foreg:foregrround "#888888" :weight bold)))))
