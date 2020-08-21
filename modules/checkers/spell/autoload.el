;;; checkers/spell/autoload.el -*- lexical-binding: t; -*-

;;;###autodef
(defalias 'flyspell-mode! #'flyspell-mode)

;;;###autodef
(defun set-flyspell-predicate! (modes predicate)
  (ignore modes predicate))
