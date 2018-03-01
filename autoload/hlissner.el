;;; private/hlissner/autoload.el -*- lexical-binding: t; -*-

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
