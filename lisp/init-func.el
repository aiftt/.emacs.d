;;; -*- lexical-binding: t; -*-


(defun max-gc-limit ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun reset-gc-limit ()
  (setq gc-cons-threshold 100000000))

;; --- system
(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

;; time date
(defun gcl/insert-standard-date ()
  "Inserts standard date time string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %T")))

(defun gcl/insert-changelog-date ()
  "Insert changelog date, like yyyy/mm/dd."
  (interactive)
  (insert (format-time-string "%Y/%m/%d")))

(defun gcl/insert-current-time ()
  "Insert current time, like hh:mm:ss."
  (interactive)
  (insert (format-time-string "%T")))

(defun gcl/consult-file-externally (file)
  "Open the FILE externally using the system's default program."
  (interactive "fFile to open externally: ")
  (cond
   ((eq system-type 'darwin) ; macOS
    (start-process "external-program" nil "open" file))
   ((eq system-type 'gnu/linux) ; Linux
    (start-process "external-program" nil "xdg-open" file))
   ((eq system-type 'windows-nt) ; Windows
    (start-process "external-program" nil "start" "" file))
   (t ; Other platforms
    (message "Opening files externally is not supported on this platform."))))


(defun gcl/open-current-directory ()
  (interactive)
  (gcl/consult-file-externally default-directory))

(defun gcl/copy-file-full-name ()
  "Copy the current buffer's file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun gcl/copy-file-name-only ()
  "Copy the current buffer's file name only to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new (file-name-nondirectory filename))
      (message "Copied buffer file name '%s' to the clipboard." (file-name-nondirectory filename)))))

(defun gcl/kill-current-buffer ()
  (interactive)
  (kill-current-buffer)
  (switch-to-prev-buffer))

(defun buf-move-up ()
  "Swap the current buffer and the buffer above the split.
If there is no split, ie now window above the current one, an
error is signaled."
  ;;  "Switches between the current buffer, and the buffer above the
  ;;  split, if possible."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'up))
	       (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No window above this one")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

(defun buf-move-down ()
  "Swap the current buffer and the buffer under the split.
If there is no split, ie now window under the current one, an
error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'down))
	       (buf-this-buf (window-buffer (selected-window))))
    (if (or (null other-win)
            (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win))))
        (error "No window under this one")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

(defun buf-move-left ()
  "Swap the current buffer and the buffer on the left of the split.
If there is no split, ie now window on the left of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'left))
	       (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No left split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

(defun buf-move-right ()
  "Swap the current buffer and the buffer on the right of the split.
If there is no split, ie now window on the right of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'right))
	       (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No right split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))


(defun kill-other-window-buffer ()
  "Kill the buffer in other window."
  (interactive)
  (other-window +1)
  (kill-this-buffer)
  (other-window -1))

(defun gcl/get-frame->selected-window ()
  "Returns a list of pairs of (frame selected-window)"
  (let* ((original-frame (window-frame))
         (result (->> (visible-frame-list)
                      (-map (lambda (f)
                              (select-frame f t)
                              (list f (selected-window)))))))
    (select-frame original-frame t)
    result))

(eval-when-compile
  (require 'cl))
(defun gcl/preserve-selected-window (f)
  "Runs the given function and then restores focus to the original window. Useful when you want to invoke
   a function (like showing documentation) but desire to keep your current window focused."
  ;; Note that we must preserve the selected window of every frame, because the function being executed may
  ;; change the focused frame, even if the current frame is in focus.
  (lexical-let* ((original-frame (selected-frame))
                 (frames->windows (gcl/get-frame->selected-window))
                 (result (funcall f)))
    (-each frames->windows (lambda (x)
                             (select-frame (first x) t)
                             (select-window (second x) t)))
    (select-frame-set-input-focus original-frame t)
    result))


(defun split-window--select-window (orig-func &rest args)
  "Switch to the other window after a `split-window'"
  (let ((cur-window (selected-window))
        (new-window (apply orig-func args)))
    (when (equal (window-buffer cur-window) (window-buffer new-window))
      (select-window new-window))
    new-window))
(advice-add 'split-window :around #'split-window--select-window)

(defun gcl/cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold  (* 1024 200)) ; 200MB
  (setq gc-cons-percentage 0.5) ; 0.5s
  (garbage-collect))

(defun switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer (startup--get-buffer-create-scratch)))

;; <f5> 刷新配置
(defun gcl/reload-init-file ()
  "Reload init file with <f5>."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(defun gcl/open-init-file()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun gcl/open-ztd-document()
  (interactive)
  (find-file "~/.gclrc/everything.org"))

(provide 'init-func)
