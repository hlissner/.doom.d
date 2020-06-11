;;; checkers/spell/config.el -*- lexical-binding: t; -*-

(defvar ispell-dictionary "en_US")

(after! ispell
  ;; Don't spellcheck org blocks
  (pushnew! ispell-skip-region-alist
            '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
            '("#\\+BEGIN_SRC" . "#\\+END_SRC")
            '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

  ;; Enable either aspell or hunspell.
  ;;   If no module flags are given, enable either aspell or hunspell if their
  ;;     binary is found.
  ;;   If one of the flags `+aspell' or `+hunspell' is given, only enable that
  ;;     spell checker.
  (pcase (cond ((featurep! +aspell)   'aspell)
               ((featurep! +hunspell) 'hunspell)
               ((executable-find "aspell")   'aspell)
               ((executable-find "hunspell") 'hunspell))
    (`aspell
     (setq ispell-program-name "aspell"
           ispell-extra-args '("--sug-mode=ultra" "--run-together" "--dont-tex-check-comments"))

     (add-hook! 'text-mode-hook
       (defun +spell-remove-run-together-switch-for-aspell-h ()
         (setq-local ispell-extra-args (remove "--run-together" ispell-extra-args))))

     (defun +spell-init-ispell-extra-args-a (orig-fun &rest args)
       :around '(ispell-word flyspell-auto-correct-word)
       (let ((ispell-extra-args (remove "--run-together" ispell-extra-args)))
         (ispell-kill-ispell t)
         (apply orig-fun args)
         (ispell-kill-ispell t))))

    (`hunspell
     (setq ispell-program-name "hunspell"))

    (_ (doom-log "Spell checker not found. Either install `aspell' or `hunspell'"))))


(use-package! spell-fu
  :hook (text-mode . spell-fu-mode)
  :config
  (pushnew! spell-fu-faces-exclude 'org-meta-line 'org-link 'org-code))
