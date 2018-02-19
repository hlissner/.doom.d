;;; private/hlissner/autoload.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+hlissner/find-in-dotfiles "autoload/hlissner" nil t)
(+default--def-find-in!   dotfiles (expand-file-name "~/.dotfiles") hlissner)
;;;###autoload (autoload '+hlissner/browse-dotfiles "autoload/hlissner" nil t)
(+default--def-browse-in! dotfiles (expand-file-name "~/.dotfiles") hlissner)
