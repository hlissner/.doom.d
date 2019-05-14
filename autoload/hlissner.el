;;; ~/.config/doom/autoload/hlissner.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +hlissner/find-in-dotfiles ()
  "Open a file somewhere in ~/.dotfiles via a fuzzy filename search."
  (interactive)
  (doom-project-find-file (expand-file-name "~/.dotfiles")))

;;;###autoload
(defun +hlissner/browse-dotfiles ()
  "Browse the files in ~/.dotfiles."
  (interactive)
  (doom-project-browse (expand-file-name "~/.dotfiles")))

;;;###autoload
(defun +hlissner/find-notes-for-major-mode (&optional arg)
  "TODO"
  (interactive "P")
  (let ((default-directory (expand-file-name "code/" org-directory)))
    (if arg
        (call-interactively #'find-file)
      (find-file
       (expand-file-name (concat (string-remove-suffix "-mode" (symbol-name major-mode)) ".org"))))))

;;;###autoload
(defun +hlissner/find-notes-for-project (&optional arg)
  "TODO"
  (interactive "P")
  (let ((project-root (doom-project-name 'nocache))
        (default-directory (expand-file-name "projects/" org-directory)))
    (if arg
        (call-interactively #'find-file)
      (find-file
       (expand-file-name (concat project-root ".org"))))))

(defvar +hlissner--recording-frame nil)
;;;###autoload
(defun +hlissner/open-recording-frame ()
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
  (set-frame-size +hlissner--recording-frame 750 300 t)
  (unless (frame-visible-p +hlissner--recording-frame)
    (make-frame-visible +hlissner--recording-frame)
    (redraw-frame +hlissner--recording-frame)
    (with-selected-frame +hlissner--recording-frame
      (keycast-mode +1))))
