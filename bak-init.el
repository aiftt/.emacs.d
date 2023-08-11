(use-package svg-lib :ensure t)

(use-package svg-tag-mode
  :disabled t
  :ensure t
  :after org
  :hook (org-mode . svg-tag-mode)
  :config
  (defun mk/svg-checkbox-empty ()
    (let* ((svg (svg-create 14 14)))
      (svg-rectangle svg 0 0 14 14 :fill 'white :rx 2 :stroke-width 2.5 :stroke-color 'black)
      (svg-image svg :ascent 'center)))

  (defun mk/svg-checkbox-filled ()
    (let* ((svg (svg-create 14 14)))
      (svg-rectangle svg 0 0 14 14 :fill "#FFFFFF" :rx 2)
      (svg-polygon svg '((5.5 . 11) (12 . 3.5) (11 . 2) (5.5 . 9) (1.5 . 5) (1 . 6.5))
                   :stroke-color 'black :stroke-width 1 :fill 'black)
      (svg-image svg :ascent 'center)))

  (defun mk/svg-checkbox-toggle ()
    (interactive)
    (save-excursion
      (let* ((start-pos (line-beginning-position))
             (end-pos (line-end-position))
             (text (buffer-substring-no-properties start-pos end-pos))
             (case-fold-search t)       ; Let X and x be the same in search
             )
        (beginning-of-line)
        (cond ((string-match-p "\\[X\\]" text)
               (progn
                 (re-search-forward "\\[X\\]" end-pos)
                 (replace-match "[ ]")))
              ((string-match-p "\\[ \\]" text)
               (progn
                 (search-forward "[ ]" end-pos)
                 (replace-match "[X]")))))))

  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                      nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag (concat value "%")
                             nil :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
           (count (float (car seq)))
           (total (float (cadr seq))))
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar (/ count total) nil
                                        :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                  (svg-lib-tag value nil
                               :stroke 0 :margin 0)) :ascent 'center)))

  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

  (setq svg-tag-tags
        `(
          ;; -- Number
          ("\([0-9a-zA-Z]\)" . ((lambda (tag)
                                  (svg-tag-make tag :beg 1 :end -1 :radius 12))))
          ;; -- Task priority
          ("\\[#[A-Z]\\]" . ((lambda (tag)
                               (svg-tag-make tag :face 'org-priority
                                             :beg 2 :end -1 :margin 0))))
          ;; -- Tags
          ("\\(:#[A-Za-z0-9]+\\)" . ((lambda (tag)
                                       (svg-tag-make tag :beg 2))))
          ("\\(:#[A-Za-z0-9]+:\\)$" . ((lambda (tag)
                                         (svg-tag-make tag :beg 2 :end -1))))

          ;; -- Progress
          ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                              (svg-progress-percent (substring tag 1 -2)))))
          ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                            (svg-progress-count (substring tag 1 -1)))))

          ;; -- Checkbox
          ("\\[ \\]" . ((lambda (_tag) (mk/svg-checkbox-empty))
                        (lambda () (interactive) (mk/svg-checkbox-toggle))
                        "Click to toggle."))
          ("\\(\\[[Xx]\\]\\)" . ((lambda (_tag) (mk/svg-checkbox-filled))
                                 (lambda () (interactive) (mk/svg-checkbox-toggle))
                                 "Click to toggle."))

          ;; -- Date: Active date (with or without day name, with or without time)
          (,(format "\\(<%s>\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0))))
          (,(format "\\(<%s \\)%s>" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
          (,(format "<%s \\(%s>\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

          ;; -- Date: Inactive date  (with or without day name, with or without time)
          (,(format "\\(\\[%s\\]\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
          (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
          (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))

          ;; Keywords
          ;; ("TODO" . ((lambda (tag) (svg-tag-make tag :height 0.8 :inverse t
          ;;                                        :face 'org-todo :margin 0 :radius 5))))
          ;; ("WORK" . ((lambda (tag) (svg-tag-make tag :height 0.8
          ;;                                        :face 'org-todo :margin 0 :radius 5))))
          ;; ("DONE" . ((lambda (tag) (svg-tag-make tag :height 0.8 :inverse t
          ;;                                        :face 'org-done :margin 0 :radius 5))))

          ("FIXME\\b" . ((lambda (tag) (svg-tag-make "FIXME" :face 'org-todo :inverse t :margin 0 :crop-right t))))
          ("DONE\\b" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :inverse t :margin 0 :crop-right t))))
          ("TODO\\b" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-tag :inverse t :margin 0 :crop-right t))))
          ("WORK\\b" . ((lambda (tag) (svg-tag-make "WORK" :face 'org-todo :inverse t :margin 0 :crop-right t))))
          ))

  (require 'org-tempo)
  (require 'org-src)
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation t
        ;; or current-window
        org-src-window-setup 'other-window)
   (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
   (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
   (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
   (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
   (add-to-list 'org-structure-template-alist '("py" . "src python"))
   (add-to-list 'org-structure-template-alist '("go" . "src go"))
   (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
   (add-to-list 'org-structure-template-alist '("json" . "src json"))

   (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (python . t)
      (shell . t)
      (js . t)))

   ;; 设置 JavaScript 代码块的默认语言为 "js"
   (setq org-babel-default-header-args:js
         '((:session . "none")
           (:results . "replace")
           (:exports . "both")
           (:cache . "no")
           (:noweb . "no")
           (:hlines . "no")
           (:tangle . "no")))
   )
