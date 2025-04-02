;;; $DOOMDIR/autoload/hlissner.el -*- lexical-binding: t; -*-

;;;###autoload
(defun find-in-dotfiles ()
  "Open a file somewhere in ~/.dotfiles via a fuzzy filename search."
  (interactive)
  (doom-project-find-file (expand-file-name "~/.config/dotfiles")))

;;;###autoload
(defun browse-dotfiles ()
  "Browse the files in ~/.dotfiles."
  (interactive)
  (doom-project-browse (expand-file-name "~/.config/dotfiles")))


;;
;;; Recording frame

(defvar +hlissner--recording-frame nil)

(defun cleanup-recording-frame (frame)
  (when (eq frame +hlissner--recording-frame)
    (with-selected-frame frame
      (doom-adjust-font-size 2 frame)
      (keycast-mode -1)
      (global-keycast-mode -1)
      (remove-hook 'delete-frame-functions #'cleanup-recording-frame))))

;;;###autoload
(defun open-recording-frame ()
  "TODO"
  (interactive)
  (unless (frame-live-p +hlissner--recording-frame)
    (setq +hlissner--recording-frame
          (make-frame '((name . "emacs-floating")
                        (visibility . nil)
                        (no-other-frame . t)
                        (desktop-dont-save . t)))))
  (with-current-buffer (get-buffer-create "*recording*")
    (emacs-lisp-mode)
    (when (featurep 'solaire-mode)
      (setq doom-real-buffer-p t)
      (solaire-mode +1))
    (erase-buffer)
    (dotimes (_ 4)
      (insert "The quick brown fox jumps over the lazy dog\n"))
    (goto-char (point-min))
    (set-window-buffer (frame-root-window +hlissner--recording-frame)
                       (current-buffer)))
  (with-selected-frame +hlissner--recording-frame
    (doom-adjust-font-size 2)
    (set-frame-size nil 840 300 t)
    (unless (frame-visible-p +hlissner--recording-frame)
      (make-frame-visible)
      (redraw-frame)
      (global-keycast-mode +1)
      (add-hook 'delete-frame-functions #'cleanup-recording-frame))))


;;
;;; Scratch frame

(defvar +hlissner--scratch-frame nil)

(defun cleanup-scratch-frame (frame)
  (when (eq frame +hlissner--scratch-frame)
    (with-selected-frame frame
      (setq doom-fallback-buffer-name (frame-parameter frame 'old-fallback-buffer))
      (remove-hook 'delete-frame-functions #'cleanup-scratch-frame))))

;;;###autoload
(defun open-scratch-frame (&optional fn)
  "Opens the org-capture window in a floating frame that cleans itself up once
you're done. This can be called from an external shell script."
  (interactive)
  (let* ((frame-title-format "")
         (preframe (cl-loop for frame in (frame-list)
                            if (equal (frame-parameter frame 'name) "xst-scratch")
                            return frame))
         (frame (unless preframe
                  (make-frame `((name . "xst-scratch")
                                (width . 140)
                                (height . 34)
                                (transient . t)
                                (internal-border-width . 10)
                                (left-fringe . 0)
                                (right-fringe . 0)
                                (undecorated . t)
                                ,(if IS-LINUX '(display . ":0")))))))
    (setq +hlissner--scratch-frame (or frame posframe))
    (select-frame-set-input-focus +hlissner--scratch-frame)
    (when frame
      (with-selected-frame frame
        (if fn
            (call-interactively fn)
          (with-current-buffer (switch-to-buffer "*scratch*")
            ;; (text-scale-set 2)
            (when (eq major-mode 'fundamental-mode)
              (emacs-lisp-mode)))
          (redisplay)
          (set-frame-parameter frame 'old-fallback-buffer doom-fallback-buffer-name)
          (setq doom-fallback-buffer-name "*scratch*")
          (add-hook 'delete-frame-functions #'cleanup-scratch-frame))))))
