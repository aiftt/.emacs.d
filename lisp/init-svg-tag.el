;;; -*- lexical-binding: t; -*-


;; https://github.com/rougier/svg-tag-mode/blob/main/examples/example-2.el

(defun svg-progress-percent (value)
  (save-match-data
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar  (/ (string-to-number value) 100.0)
                                       nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag (concat value "%")
                             nil :stroke 0 :margin 0)) :ascent 'center)))

(defun svg-progress-count (value)
  (save-match-data
    (let* ((seq (split-string value "/"))
           (count (if (stringp (car seq))
                      (float (string-to-number (car seq)))
                    0))
           (total (if (stringp (cadr seq))
                      (float (string-to-number (cadr seq)))
                    1000)))
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar (/ count total) nil
                                        :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                  (svg-lib-tag value nil
                               :stroke 0 :margin 0)) :ascent 'center))))

(use-package svg-tag-mode
  :ensure t
  :after org
  :hook (org-mode . svg-tag-mode)
  :config

  ;; (plist-put svg-lib-style-default :font-family "JetBrainsMono Nerd Font")
  ;; (plist-put svg-lib-style-default :font-size 13)
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst duration-re "[0-9]\\{1\\}:[0-9]\\{2\\}$")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))
  (defun svg-progress-percent (value)
    (save-match-data
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar  (/ (string-to-number value) 100.0)
                                         nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                  (svg-lib-tag (concat value "%")
                               nil :stroke 0 :margin 0)) :ascent 'center)))
  (defun svg-progress-count (value)
    (save-match-data
      (let* ((seq (split-string value "/"))
             (count (if (stringp (car seq))
                        (float (string-to-number (car seq)))
                      0))
             (total (if (stringp (cadr seq))
                        (float (string-to-number (cadr seq)))
                      1000)))
        (svg-image (svg-lib-concat
                    (svg-lib-progress-bar (/ count total) nil
                                          :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                    (svg-lib-tag value nil
                                 :stroke 0 :margin 0)) :ascent 'center))))
  (setq svg-tag-tags
        `(
          ;; TODO / DONE
          ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face '(:foreground "IndianRed" :weight bold) :inverse t :margin 0 :height 0.8))))
          ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :inverse t :margin 0 :height 0.8))))
          ("BLOCK" . ((lambda (tag) (svg-tag-make "BLOCK" :face "red" :inverse t :margin 0 :height 0.8))))
          ("DOING" . ((lambda (tag) (svg-tag-make "DOING" :face "green" :inverse t :margin 0 :height 0.8))))
          ("PAUSED" . ((lambda (tag) (svg-tag-make "PAUSED" :face "yellow" :inverse t :margin 0 :height 0.8))))
          ("CANC" . ((lambda (tag) (svg-tag-make "CANC" :face '(:foreground "DarkGrey" :weight bold :strike-through t) :inverse t :margin 0 :height 0.8))))

          ;; ("^\s*#\\+title:" . ((lambda (tag) (svg-tag-make :face ))))

          ;; Task priority
          ;; ("\\[#A\\]" . ((lambda (tag) (svg-tag-make "(󰄿)" :face '(:foreground "IndianRed" :weight bold) :inverse t :height 0.8))))
	        ;; ("\\[#B\\]" . ((lambda (tag) (svg-tag-make "(󰅃)" :face '(:foreground "DarkOrange" :weight bold) :inverse t :height 0.8))))
	        ;; ("\\[#C\\]" . ((lambda (tag) (svg-tag-make "(󰅀)" :face '(:foreground "Grey" :weight bold) :inverse t :height 0.8))))
          ("\\[#[A-Z]\\]" . ( (lambda (tag)
                                (svg-tag-make tag :face 'org-priority
                                              :beg 2 :end -1 :inverse t :margin 0 :height 0.8))))
          ("\([0-9a-zA-Z]\)" . ((lambda (tag)
                                  (svg-tag-make tag :beg 1 :end -1 :radius 12))))
          ("|[0-9a-zA-Z-]+?|" . ((lambda (tag)
                                   (svg-tag-make tag :face 'font-lock-comment-face
                                                 :margin 0 :beg 1 :end -1))))

          (":NOTE:" . ((lambda (tag)
                         (svg-tag-make tag :beg 1 :end -1 :face 'font-lock-comment-face
                                       :inverse nil :margin 0 :radius 0))))

          ;; Active date (with or without day name, with or without time)


          (,(format "\\(<%s>\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0 :height 0.8))))
          (,(format "\\(<%s \\)%s>" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :height 0.8))))
          (,(format "<%s \\(%s>\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :height 0.8))))

          ;; Inactive date  (with or without day name, with or without time)
          (,(format "\\(%s\\)" duration-re) .
           ((lambda (tag)
              (svg-tag-make tag :face 'org-date :inverse t :margin 0 :height 0.8))))

          (,(format "\\(\\[%s\\]\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date :height 0.8))))
          (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date :height 0.8))))
          (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date :height 0.8))))

          ;; ;; Progress
          ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                              (svg-progress-percent (substring tag 1 -2)))))
          ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                            (svg-progress-count (substring tag 1 -1)))))

          ;; tag, eg. :#tag1:#tag2:$
          ("\\(:#[A-Za-z0-9]+\\)" . ((lambda (tag)
                                       (svg-tag-make tag :beg 2))))
          ("\\(:#[A-Za-z0-9]+:\\)$" . ((lambda (tag)
                                         (svg-tag-make tag :beg 2
                                                       :end -1))))

          ;; This replaces any occurrence of ":HELLO:" with a static SVG tag that
          ;; can be clicked to execute the specified command. Help message is
          ;; displayed when the tag is hovered with the pointer.
          (":HELLO:" .  ((lambda (tag) (svg-tag-make "HELLO"))
                         (lambda () (interactive) (message "Hello world!"))
                         "Print a greeting message"))
          )))

(provide 'init-svg-tag)
