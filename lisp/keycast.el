;;; /mnt/projects/conf/doom-emacs-private/lisp/keycast.el -*- lexical-binding: t; -*-

(defvar keycast-blacklist
  '(nil self-insert-command backward-char forward-char
        digit-argument
        delete-char delete-backward-char backward-delete-char
        backward-delete-char-untabify
        universal-argument universal-argument-other-key
        universal-argument-minus universal-argument-more
        beginning-of-line end-of-line recenter
        move-end-of-line move-beginning-of-line
        handle-switch-frame
        newline previous-line next-line)
  "A list commands which should not be logged, despite logging being enabled.

Frequently used non-interesting commands (like cursor movements) should be put
here.")

(defconst keycast-deferring-commands
  '(evil-yank
    evil-cp-yank
    evil-sp-yank
    lispyville-yank
    evil-delete
    evil-cp-delete
    evil-sp-delete
    lispyville-delete
    evil-change
    +eval:region)
  "TODO")

(defvar keycast-column 11)
(defvar keycast-insert-fn #'keycast-insert-command)
(defvar keycast-display-fn #'keycast-display-popup)
(defvar keycast-cleanup-hook nil)
(defvar keycase-buffer-name "*keycast*")
(defvar keycast-width 40)
(defvar keycast-height 25)

(defvar keycast--deferred nil)
(defvar keycast--frame nil)
(defvar keycast--dim-timer nil)
(defvar keycast--last-operator nil)


(defun keycast--buffer ()
  (get-buffer-create keycase-buffer-name))

(defun keycast--dim (pos fg bg max)
  (with-current-buffer (keycast--buffer)
    (let ((line (line-number-at-pos pos))
          color)
      (while (and (not (eobp))
                  (< line max))
        (setq color (doom-blend bg fg (min 1.0 (/ line (float (1- max))))))
        (set-text-properties (line-beginning-position) (line-end-position) `(face (:foreground ,color :weight normal)))
        (forward-line 1)
        (cl-incf line)))))

(defun keycast--fontify ()
  ;; TODO do this with font-lock instead
  (let ((max (* 1.5 (frame-char-height keycast--frame)))
        (fg (face-foreground 'default))
        (bg (face-foreground 'font-lock-comment-face))
        (line 0))
    (save-excursion
      (goto-char (point-min))
      (set-text-properties (line-beginning-position)
                           (line-end-position)
                           `(face (:foreground ,(face-background 'highlight) :weight bold)))
      (forward-line 1)
      (keycast--dim (point) fg bg max))
    (when (timerp keycast--dim-timer)
      (cancel-timer keycast--dim-timer))
    (setq keycast--dim-timer (run-at-time 1 nil #'keycast--dim (point-min) fg bg max))))

(defun keycast-insert-command (keystr command &optional arg)
  (with-current-buffer (keycast--buffer)
    (goto-char (point-min))
    (save-excursion (insert "\n"))
    (when arg
      (insert (format "%s "
                      (if (integerp arg)
                          arg
                        (universal-argument--description)))))
    (insert (propertize (key-description keystr) :time (current-time)))
    (when (>= (current-column) keycast-column)
      (newline))
    (move-to-column keycast-column t)
    (if (listp command)
        (while command
          (when-let* ((cmd (pop command)))
            (princ (if (byte-code-function-p cmd) "<bytecode>" cmd)
                   (current-buffer))
            (when (delq nil command)
              (insert " -> "))))
      (princ (if (byte-code-function-p command) "<bytecode>" command)
             (current-buffer)))
    (keycast--fontify)))

(defmacro keycast-save-command (&rest body)
  `(let ((deactivate-mark nil) ; do not deactivate mark in transient mark mode
         ;; do not let random commands scribble over {THIS,LAST}-COMMAND
         (this-command this-command)
         (last-command last-command))
     ,@body))

(defun keycast-log-command (&optional cmd)
  (keycast-save-command
   (let ((cmd (or cmd this-command))
         (keys (this-command-keys)))
     (cond ((memq cmd keycast-blacklist))
           ((and (vectorp keys)
                 (mouse-event-p (aref keys 0))))
           ((memq cmd keycast-deferring-commands)
            (setq keycast--deferred keys))
           ((funcall keycast-insert-fn keys cmd prefix-arg))))))

(defun keycast-log-post-command (&optional cmd)
  (when keycast--deferred
    (keycast-save-command
     (let ((cmd (or cmd this-command))
           (keys (this-command-keys)))
       (funcall keycast-insert-fn
                (concat keycast--deferred keys)
                (list cmd
                      (pcase (substring keys 0 1)
                        ("i" (lookup-key evil-inner-text-objects-map (substring keys 1) t))
                        ("a" (lookup-key evil-outer-text-objects-map (substring keys 1) t))))
                current-prefix-arg)
       (setq keycast--deferred nil)))))


;;
;;; Commands

(defun keycast-display-popup ()
  (with-selected-window
      (display-buffer-in-side-window (keycast--buffer)
                                     `((side . right)
                                       (window-width . ,keycast-width)))
    (setq mode-line-format nil)))

(defun keycast-display-childframe ()
  (unless (frame-live-p keycast--frame)
    (setq keycast--frame
          (make-frame
           `((parent-frame . ,(window-frame))
             (fullscreen . nil)
             (no-accept-focus . t)
             (min-width  . 0)
             (min-height . 0)
             (border-width . 2)
             (internal-border-width . 6)
             (vertical-scroll-bars . nil)
             (horizontal-scroll-bars . nil)
             (left-fringe . 0)
             (right-fringe . 0)
             (menu-bar-lines . 0)
             (tool-bar-lines . 0)
             (line-spacing . 0)
             (unsplittable . t)
             (no-other-frame . t)
             (undecorated . t)
             (visibility . nil)
             (cursor-type . nil)
             (minibuffer . nil)
             (no-special-glyphs . t)
             (desktop-dont-save . t)
             (background-color . ,(face-background 'default)))))
    (let ((win (frame-root-window keycast--frame)))
      ;; This method is more stable than 'setq mode/header-line-format nil'
      (set-window-parameter win 'mode-line-format 'none)
      (set-window-parameter win 'header-line-format 'none)
      (set-window-buffer win (keycast--buffer))
      (set-window-dedicated-p win t)))
  (set-frame-size keycast--frame
                  (- (* keycast-width (frame-char-width)) 16 5)
                  (- (frame-pixel-height) (window-mode-line-height) (frame-char-height) 16 10)
                  t)
  (set-frame-position keycast--frame -5 5)
  (unless (frame-visible-p keycast--frame)
    (make-frame-visible keycast--frame)
    (redraw-frame keycast--frame)))

(defun keycast-clear-buffer ()
  "Clear the keycast buffer."
  (interactive)
  (with-current-buffer (keycast--buffer)
    (erase-buffer)))

(defun keycast-cleanup ()
  (interactive)
  (when (and (eq keycast-display-fn #'keycast-display-childframe)
             (frame-live-p keycast--frame))
    (delete-frame keycast--frame))
  (with-current-buffer (keycast--buffer)
    (run-hooks 'keycast-cleanup-hook)
    (kill-buffer (current-buffer))))

(defvar keycast-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-r") #'keycast-start-recording)
    (define-key map (kbd "C-c C-c") #'keycast-finish-recording)
    (define-key map (kbd "C-c C-k") #'keycast-abort-recording)
    map))

(define-minor-mode keycast-mode
  :global t
  :init-value nil
  (if keycast-mode
      (progn
        (keycast-clear-buffer)
        (funcall keycast-display-fn)
        (add-hook 'pre-command-hook #'keycast-log-command)
        (add-hook 'post-command-hook #'keycast-log-post-command))
    (remove-hook 'pre-command-hook #'keycast-log-command)
    (remove-hook 'post-command-hook #'keycast-log-post-command)
    (keycast-cleanup)))


;;
;;; Screencasting

(defvar keycast--current-filename nil)
(defvar keycast--process nil)

(defun keycast--process-sentinel (process _event)
  (when (memq (process-status process) '(exit stop))
    (setq keycast--process nil)
    (ignore-errors (kill-buffer "*ffmpeg*"))
    (kill-new keycast--current-filename)
    (message "Saved to %s" (abbreviate-file-name keycast--current-filename))
    (when (y-or-n-p "Preview file?")
      (async-shell-command (format "mpv %S" keycast--current-filename)))
    (setq keycast--current-filename nil)))

(defun keycast-start-recording ()
  (interactive)
  (when (process-live-p keycast--process)
    (user-error "Already recording"))
  (keycast-clear-buffer)
  (cl-destructuring-bind (left top _right _bottom)
      (frame-edges)
    (let ((dest (format (expand-file-name "~/screenshots/%s-%s.mp4")
                        (format-time-string "%F-%T")
                        (replace-regexp-in-string "[^[:alnum:]]" "" (buffer-name)))))
      (setq keycast--current-filename dest
            keycast--process
            (make-process :name "ffmpeg"
                          :buffer (get-buffer-create "*ffmpeg*")
                          :command (list "ffmpeg" "-y" "-f" "x11grab"
                                         "-ss" "1"
                                         "-s" (format "%dx%d" (frame-pixel-width) (frame-pixel-height))
                                         "-i" (format ":0.0+%d,%d" left top)
                                         "-framerate" "30"
                                         keycast--current-filename)))
      (set-process-sentinel keycast--process #'keycast--process-sentinel)
      (add-to-list 'global-mode-string "▶ "))))

(defun keycast-finish-recording ()
  (interactive)
  (unless (process-live-p keycast--process)
    (user-error "No recording is active"))
  (signal-process keycast--process 'SIGINT)
  (setq global-mode-string (remove "▶ " global-mode-string))
  (message "Saving..."))

(global-set-key [remap global-command-log-mode] #'keycast-mode)
