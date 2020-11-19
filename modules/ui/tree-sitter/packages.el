;; -*- no-byte-compile: t; -*-
;;; ui/tree-sitter/packages.el

(package! tree-sitter
  :ignore (null (bound-and-true-p module-file-suffix))
  :pin "dc9094b5066ec3c4ad583ed285c282f5811323b5")
(package! tree-sitter-langs
  :ignore (null (bound-and-true-p module-file-suffix))
  :pin "dc9094b5066ec3c4ad583ed285c282f5811323b5")
