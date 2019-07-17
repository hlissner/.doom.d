;;; ~/.config/doom/packages.el

;; `emacs-snippets' *is* my personal snippets library (in $DOOMDIR/snippets)
(package! emacs-snippets :disable t)

;; I never use it.
(package! which-key :disable t)

;;
;; (package! persp-mode :ignore t)

;; For personal modeline
(package! anzu)
(package! evil-anzu)

(package! flycheck-moonscript
  :recipe (:host github :repo "hlissner/emacs-flycheck-moonscript"))
