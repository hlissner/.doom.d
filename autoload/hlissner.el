;;; ~/.config/doom/autoload/hlissner.el -*- lexical-binding: t; -*-

;;;###autoload
(defun find-in-dotfiles ()
  "Open a file somewhere in ~/.dotfiles via a fuzzy filename search."
  (interactive)
  (doom-project-find-file (expand-file-name "~/.dotfiles")))

;;;###autoload
(defun browse-dotfiles ()
  "Browse the files in ~/.dotfiles."
  (interactive)
  (doom-project-browse (expand-file-name "~/.dotfiles")))

(defvar +hlissner--recording-frame nil)

(defun cleanup-recording-frame (frame)
  (when (eq frame +hlissner--recording-frame)
    (with-selected-frame frame
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
    (set-frame-size nil 840 300 t)
    (doom-adjust-font-size 2)
    (unless (frame-visible-p +hlissner--recording-frame)
      (make-frame-visible)
      (redraw-frame)
      (keycast-mode +1)
      (add-hook 'delete-frame-functions #'cleanup-recording-frame))))
