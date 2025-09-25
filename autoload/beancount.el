;;; $DOOMDIR/autoload/ledger.el -*- lexical-binding: t; -*-

;;;###autoload
(defun beancount/fava ()
  (interactive)
  (require 'beancount)
  (when beancount--fava-process
    (delete-process beancount--fava-process)
    (setq beancount--fava-process nil)
    (message "Fava process killed"))
  (setq beancount--fava-process
        (with-temp-buffer
          (setq-local default-directory "~/projects/ledger")
          (envrc--update)
          (start-process "fava" (get-buffer-create "*fava*") "fava"
                         (doom-path "main.bean"))))
  (set-process-filter beancount--fava-process #'beancount--fava-filter)
  (message "Fava process started"))
