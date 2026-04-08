;;; $DOOMDIR/init.el -*- lexical-binding: t; -*-

(doom! ;;((sources)
       ;; (flags))

       :completion
       (corfu +orderless +dabbrev +icons)
       (vertico +icons)

       :ui
       (doom +tabs)
       dashboard
       modeline
       hl-todo
       indent-guides
       ophints
       (popup +defaults)
       (vc-gutter +pretty)
       (workspaces +tabs)

       :editor
       (evil +everywhere)
       file-templates
       fold
       format
       multiple-cursors
       rotate-text
       snippets
       (whitespace +guess +trim)

       :emacs
       (dired +dirvish +icons)
       electric
       tramp
       undo
       vc

       :term
       ;; eshell
       vterm

       :checkers
       (syntax +childframe)
       spell

       :tools
       ;; debugger
       direnv
       editorconfig
       (eval +overlay)
       (lookup +docsets +dictionary)
       llm
       (lsp +eglot)
       (magit +childframe)
       pdf
       tree-sitter
       ;; upload

       :os
       ;; tty

       :lang
       beancount
       common-lisp
       (cc +lsp +tree-sitter)
       emacs-lisp
       (gdscript +lsp +tree-sitter)
       janet
       (javascript +lsp +tree-sitter)
       (json +tree-sitter)
       (lua +lsp +tree-sitter)
       (markdown +tree-sitter)
       (nix +lsp +tree-sitter)
       (org +dragndrop +roam2 +pretty +forge +jupyter +gnuplot)
       (python +lsp +tree-sitter +uv +pyright)
       (rust +tree-sitter)
       (sh +powershell +fish +lsp)
       (yaml +tree-sitter)
       ruby
       web
       (zig +lsp +tree-sitter)

       :app
       everywhere

       :config
       ;; literate
       (default +bindings +gnupg))
