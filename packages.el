;; -*- no-byte-compile: t; -*-
;;; ~/.config/doom/packages.el

;; I already use this snippets library (in $DOOMDIR/snippets)
(package! emacs-snippets :disable t)

;; Only relevant on MacOS. exec-path-from-shell isn't installed on
;; Linux/Windows. I use bin/doom patch-macos, which renders exec-path-from-shell
;; redundant.
(package! exec-path-from-shell :disable t)
