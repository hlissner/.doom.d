;;; $DOOMDIR/init.el -*- lexical-binding: t; -*-

(doom! ;;((sources)
       ;; (flags))

       :doom
       cli

       :completion
       (corfu +orderless +dabbrev +icons)
       vertico

       :ui
       (doom +tabs)
       dashboard
       modeline
       hl-todo
       indent-guides
       ophints
       (popup +defaults)
       (vc-gutter +pretty)
       workspaces

       :editor
       (evil +everywhere)
       file-templates
       fold
       format
       multiple-cursors
       rotate-text
       snippets
       (whitespace +guess +trim)
       word-wrap

       :emacs
       (dired +dirvish)
       electric
       tramp
       undo
       vc

       :term
       ghostel
       ;; eshell
       ;; vterm

       :checkers
       (syntax +childframe)
       spell

       :tools
       debugger
       direnv
       editorconfig
       (eval +overlay)
       (lookup +docsets +dictionary)
       llm
       (lsp +eglot)
       (magit +childframe +forge)
       pdf
       tree-sitter
       ;; upload

       :os
       ;; tty

       :lang
       beancount
       ;; common-lisp
       ;; (clojure +tree-sitter)
       (cc +lsp +tree-sitter)
       emacs-lisp
       (gdscript +lsp +tree-sitter)
       janet
       ;; (latex +cdlatex +lsp +fold)
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
       ;; web
       (zig +lsp +tree-sitter)

       :app
       everywhere

       :config
       ;; literate
       (default +bindings +gnupg +smartparens))
