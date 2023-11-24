;;; -*- lexical-binding: t; -*-

;; --- edit
(require 'duplicate-line)

(use-package expand-region
  :ensure t)
(global-set-key (kbd "C-=") 'er/expand-region)

(use-package autorevert
  :diminish
  :hook (after-init . global-auto-revert-mode))
(use-package hungry-delete
  :ensure t
  :diminish
  :config
  (setq-default hungry-delete-chars-to-skip " \t\f\v"))
(global-hungry-delete-mode)

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t))

(use-package highlight-thing :ensure t)
(global-highlight-thing-mode)
(use-package symbol-overlay :ensure t)
(use-package move-text :ensure t)
(use-package iedit :ensure t)
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))
(use-package toggle-quotes-plus
  :init (slot/vc-install :repo "jcs-elpa/toggle-quotes-plus")
  :config
  (setq toggle-quotes-plus-chars '("\""
                                   "'"
                                   "`")))
(global-set-key (kbd "C-'") #'toggle-quotes-plus)

(use-package maple-iedit
  :init (slot/vc-install :repo "honmaple/emacs-maple-iedit")
  :commands (maple-iedit-match-all maple-iedit-match-next maple-iedit-match-previous)
  :config
  (setq maple-iedit-ignore-case t)

  (defhydra maple/iedit ()
    ("n" maple-iedit-match-next "next")
    ("t" maple-iedit-skip-and-match-next "skip and next")
    ("T" maple-iedit-skip-and-match-previous "skip and previous")
    ("p" maple-iedit-match-previous "prev"))
  :bind (:map evil-visual-state-map
              ("n" . maple/iedit/body)
              ("C-n" . maple-iedit-match-next)
              ("C-p" . maple-iedit-match-previous)
              ("C-t" . maple-iedit-skip-and-match-next)))

(use-package string-inflection
  :ensure t)

(defun my-string-inflection-cycle-auto ()
  "switching by major-mode"
  (interactive)
  (cond
   ;; for emacs-lisp-mode
   ((eq major-mode 'emacs-lisp-mode)
    (string-inflection-all-cycle))
   ;; for python
   ((eq major-mode 'python-mode)
    (string-inflection-python-style-cycle))
   ;; for java
   ((eq major-mode 'java-mode)
    (string-inflection-java-style-cycle))
   ;; for elixir
   ((eq major-mode 'elixir-mode)
    (string-inflection-elixir-style-cycle))
   (t
    ;; default
    (string-inflection-ruby-style-cycle))))

(use-package parrot
  :ensure t
  :config
  (parrot-mode -1)
  (setq parrot-rotate-dict
        '(
          (:rot ("alpha" "beta") :caps t :lower nil)
          ;; => rotations are "Alpha" "Beta"

          (:rot ("snek" "snake" "stawp"))
          ;; => rotations are "snek" "snake" "stawp"

          (:rot ("yes" "no") :caps t :upcase t)
          ;; => rotations are "yes" "no", "Yes" "No", "YES" "NO"

          (:rot ("&" "|"))
          ;; => rotations are "&" "|"

          ;; default dictionary starts here ('v')
          (:rot ("begin" "end") :caps t :upcase t)
          (:rot ("enable" "disable") :caps t :upcase t)
          (:rot ("enter" "exit") :caps t :upcase t)
          (:rot ("forward" "backward") :caps t :upcase t)
          (:rot ("front" "rear" "back") :caps t :upcase t)
          ;; (:rot ("get" "set") :caps t :upcase t)
          (:rot ("high" "low") :caps t :upcase t)
          (:rot ("in" "out") :caps t :upcase t)
          (:rot ("left" "right") :caps t :upcase t)
          (:rot ("min" "max") :caps t :upcase t)
          (:rot ("on" "off") :caps t :upcase t)
          (:rot ("prev" "next"))
          (:rot ("start" "stop") :caps t :upcase t)
          (:rot ("true" "false") :caps t :upcase t)
          (:rot ("&&" "||"))
          (:rot ("==" "!="))
          (:rot ("." "->"))
          (:rot ("if" "else" "elif"))
          (:rot ("ifdef" "ifndef"))
          (:rot ("int8_t" "int16_t" "int32_t" "int64_t"))
          (:rot ("uint8_t" "uint16_t" "uint32_t" "uint64_t"))
          (:rot ("1" "2" "3" "4" "5" "6" "7" "8" "9" "10"))
          (:rot ("1st" "2nd" "3rd" "4th" "5th" "6th" "7th" "8th" "9th" "10th"))

          ;; mine
          (:rot ("let" "const" "var"))
          (:rot ("sm" "md" "lg" "xl" "2xl" "3xl"))
          (:rot ("aspect-auto" "aspect-square" "aspect-video"))
          (:rot ("break-after-auto" "break-after-avoid" "break-after-all" "break-after-avoid-page" "break-after-page" "break-after-left" "break-after-right" "break-after-column"))
          (:rot ("box-border" "box-content"))
          (:rot ("block" "inline-block" "inline" "flex" "inline-flex" "table" "inline-table" "table-caption" "table-cell" "table-column" "table-column-group" "table-footer-group" "table-header-group" "table-row-group" "table-row" "flow-root" "grid" "inline-grid" "contents" "list-item" "hidden"))
          (:rot ("float-right" "float-left" "float-none"))
          (:rot ("clear-left" "clear-right" "clear-both" "clear-none"))
          (:rot ("object-contain" "object-cover" "object-fill" "object-none" "object-scale-down"))
          (:rot ("object-bottom" "object-center" "object-left" "object-left-bottom" "object-left-top" "object-right" "object-right-bottom" "object-right-top" "object-top"))
          (:rot ("overflow-auto" "overflow-hidden" "overflow-clip" "overflow-visible" "overflow-scroll"))
          (:rot ("static" "fixed" "absolute" "relative" "sticky"))
          (:rot ("visible" "invisible" "collapse"))
          (:rot ("flex-row" "flex-row-reverse" "flex-col" "flex-col-reverse"))
          (:rot ("flex-wrap" "flex-wrap-reverse" "flex-nowrap"))
          (:rot ("flex-1" "flex-auto" "flex-initial" "flex-none"))
          (:rot ("grow" "grow-0"))
          (:rot ("shrink" "shrink-0"))
          (:rot ("get" "post" "set") :caps t :upcase t)
          )))

;; aspect-video

(use-package separedit
  :ensure t)

(defun separedit//region-of-swagger-commentary ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "* @swagger\n")
      (let ((begin (point)))
        (when (re-search-forward  "\s+\\*/" nil t)
          (goto-char (match-beginning 0))
          (list begin (point)))))))

(defun separedit/edit-swagger-commentary ()
  "Edit whole commentary section as a single block."
  (interactive)
  (let ((separedit-leave-blank-line-in-comment t))
    (separedit-dwim
     (apply #'separedit-mark-region
            `(,@(separedit//region-of-swagger-commentary)
              yaml-mode)))))



(provide 'init-edit)
