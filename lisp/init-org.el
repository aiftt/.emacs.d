;;; -*- lexical-binding: t; -*-


;; --- org
(with-eval-after-load 'org
  (progn
    (require 'org-tempo)

    (setq org-directory "~/.gclrc/org/"
          org-startup-indented t
          org-pretty-entities t
          org-hide-emphasis-markers t
          org-startup-with-inline-images t
          org-image-actual-width '(300)
          org-html-doctype "html5")
    )

  ;; -- 使用 “+” 来切换列表风格，- -> 1. -> a. ...
  (evil-define-key 'normal org-mode-map
    "+" #'org-cycle-list-bullet)

  ;; -- keywords
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)" "MEETING(m)" "PHONE(p)"))))

  ;; -- paint
  (setq org-plantuml-jar-path "~/.gclrc/plantuml.jar")
  (setq org-ditaa-jar-path "~/.gclrc/ditaa.jar")

  ;; -- emphasis
  (setq org-emphasis-alist
        '(("*" my-org-emphasis-bold)
          ("/" my-org-emphasis-italic)
          ("_" underline)
          ("=" org-verbatim verbatim)
          ("~" org-code verbatim)
          ("+" (:strike-through t))))
  (defface my-org-emphasis-bold
    '((default :inherit bold)
      (((class color) (min-colors 88) (background light))
       :foreground "#a60000")
      (((class color) (min-colors 88) (background dark))
       :foreground "#ff8059"))
    "My bold emphasis for Org.")

  (defface my-org-emphasis-italic
    '((default :inherit italic)
      (((class color) (min-colors 55) (background light))
       :foreground "#972500")
      (((class color) (min-colors 55) (background dark))
       :foreground "#ef8b50"))
    "My italic emphasis for Org.")

  ;; Allow multiple line Org emphasis markup.
  ;; http://emacs.stackexchange.com/a/13828/115
  (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just 1
  ;; Below is needed to apply the modified `org-emphasis-regexp-components'
  ;; settings from above.
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  )

(use-package org-super-agenda
  :ensure t
  :config
  (defvar org-agenda-dir ""
    "gtd org files location")

  (defvar deft-dir ""
    "deft org files locaiton")

  (setq org-agenda-dir "~/.gclrc/org/")
  (setq deft-dir  "~/.gclrc/org/")
  (setq org-agenda-log-mode-items '(clock closed state))

  (setq org-agenda-inhibit-startup t) ;; ~50x speedup
  (setq org-agenda-span 'day)
  (setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
  (setq org-agenda-window-setup 'current-window)
  (setq org-log-done t)
  (setq org-columns-default-format "%60ITEM(Task) %6Effort(Estim){:}")

  (setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
  (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
  (setq org-agenda-file-work (expand-file-name "work.org" org-agenda-dir))
  (setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
  (setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
  (setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
  (setq org-agenda-file-blogposts (expand-file-name "all-posts.org" org-agenda-dir))
  (setq org-agenda-files (list org-agenda-file-gtd
                               org-agenda-file-journal org-agenda-file-blogposts
                               org-agenda-file-work org-agenda-file-note))
  )


;; https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org
(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () org-superstar-mode))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package svg-tag-mode
  :ensure t
  :after org
  :hook (org-mode . svg-tag-mode)
  :config

  (setq svg-tag-tags
        '(
          ("\([0-9a-zA-Z]\)" . ((lambda (tag)
                                  (svg-tag-make tag :beg 1 :end -1 :radius 12))))
          ))
  )

;; appear
(use-package org-appear
  :ensure t
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (defun org-apperance-evil-hack ()
    (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
    (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t))
  (setq org-appear-trigger 'manual)
  (add-hook 'org-mode-hook #'org-apperance-evil-hack)
  )

(use-package org-superstar
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  (setq org-superstar-special-todo-items t)
  )

(use-package org-mac-link
  :ensure t)

(use-package org-fragtog
  :ensure t
  :hook (org-mode . org-fragtog-mode))

;; org-download
(use-package org-download
  :ensure t
  :hook (dired-mode . org-download-enable)
  :config
  (setq-default org-download-heading-lvl nil
                org-download-image-dir "~/.img/tmp/"
                ;; org-download-screenshot-method "screencapture -i %s"
                org-download-screenshot-method "pngpaste %s"
                org-download-screenshot-file (expand-file-name "screenshot.jpg" temporary-file-directory))
  )


;; --- org-roam
(use-package org-roam
  :ensure t
  :after org
  :custom
  (org-roam-directory (file-truename user-blog-posts))
  (org-roam-dailies-directory "daily/")
  :config
  (defun gcl/org-capture-slipbox ()
    (interactive)
    (org-capture nil "s"))
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (setq org-roam-capture-templates
        '(("m" "main" plain "%?"
           :if-new (file+head "main/${slug}.org"
                              "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("x" "reference" plain "%?"
           :if-new
           (file+head "reference/${title}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("o" "others" plain "%?"
           :if-new
           (file+head "others/${title}.org"
                      "#+title: ${title}\n#+created: %U\n#+filetags: :others:\n#+startup: overview hideblocks")
           :immediate-finish t
           :unnarrowed t)
          ("v" "vue" plain "%?"
           :if-new
           (file+head "vue/${title}.org"
                      "#+title: ${title}\n#+created: %U\n#+filetags: :vue:\n#+startup: overview hideblocks\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "react" plain "%?"
           :if-new
           (file+head "react/${title}.org"
                      "#+title: ${title}\n#+created: %U\n#+filetags: :react:\n#+startup: overview hideblocks")
           :immediate-finish t
           :unnarrowed t)
          ("w" "web" plain "%?"
           :if-new
           (file+head "web/${title}.org"
                      "#+title: ${title}\n#+created: %U\n#+filetags: :web:\n#+startup: overview hideblocks")
           :immediate-finish t
           :unnarrowed t)
          ("e" "emacs" plain "%?"
           :if-new
           (file+head "emacs/${title}.org"
                      "#+title: ${title}\n#+created: %U\n#+filetags: :emacs:\n#+startup: overview hideblocks")
           :immediate-finish t
           :unnarrowed t)
          ))

  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  )

;; org-roam-ui
(use-package org-roam-ui
  :ensure t
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil
	      org-roam-ui-browser-function #'browse-url
	      ))

;; --- org-roam bindings
(general-define-key
 "C-c n n" 'org-roam-node-find
 "C-c n g" 'org-roam-graph
 "C-c n r" 'org-roam-node-random
 "C-c n c" 'org-roam-capture
 "C-c n d" 'org-roam-dailies-capture-today
 "C-c n u" 'org-roam-ui-open
 "C-c n f" 'consult-org-roam-file-find
 "C-c n b" 'consult-org-roam-backlinks
 "C-c n l" 'consult-org-roam-forward-links
 "C-c n s" 'consult-org-roam-search
 )

(general-def org-mode-map
  "C-c n i" 'org-roam-node-insert
  "C-c n o" 'org-id-get-create
  "C-c n t" 'org-roam-tag-add
  ;; "C-c n e" 'org-roam-extract-subtree
  "C-c n a" 'org-roam-alias-add
  "C-c n ," 'org-roam-buffer-toggle)

;; org bindings
(evil-define-key 'normal org-mode-map
  "tt" 'org-todo
  "tp" 'org-priority
  "td" 'org-deadline
  "tc" 'org-capture
  "tl" 'org-store-link
  "tn" 'org-add-note
  "t," 'org-toggle-checkbox

  ;; clock
  "ci" 'org-clock-in
  "co" 'org-clock-out
  "cg" 'org-clock-goto
  "cx" 'org-clock-cancel
  "ck" 'org-clock-timestamps-up
  "cj" 'org-clock-timestamps-down
  )


(provide 'init-org)
