;;; tools/direnv/config.el -*- lexical-binding: t; -*-

(defvar +direnv-keywords
  '("direnv_layout_dir" "PATH_add" "path_add" "log_status" "log_error" "has"
    "join_args" "expand_path" "dotenv" "user_rel_path" "find_up" "source_env"
    "watch_file" "source_up" "direnv_load" "MANPATH_add" "load_prefix" "layout"
    "use" "rvm" "use_nix" "use_guix")
  "TODO")


;;
;;; Packages

(use-package! envrc
  :when (executable-find "direnv")
  :hook (doom-first-file . envrc-global-mode)
  :mode ("\\.envrc\\'" . +direnv-rc-mode)
  :config
  (define-derived-mode +direnv-rc-mode sh-mode "envrc"
    "Major mode for .envrc files."
    ;; Fontify special .envrc keywords; it's a good indication of whether or not
    ;; we've typed them correctly
    (font-lock-add-keywords
     nil `((,(regexp-opt +direnv-keywords 'symbols)
            (0 font-lock-keyword-face)))))

  (defadvice! +direnv--fail-gracefully-a (&rest _)
    "Don't try to use direnv if the executable isn't present."
    :before-while #'envrc-mode
    (or (executable-find "direnv")
        (ignore (doom-log "Couldn't find direnv executable")))))
