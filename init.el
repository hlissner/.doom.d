;;; $DOOMDIR/init.el -*- lexical-binding: t; -*-

(doom! ;;((sources)
       ;; (flags))

       :completion
       (corfu +dabbrev +icons)
       (vertico +icons)

       :ui
       (doom +tabs)
       modeline
       doom-dashboard
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
       ;; eww
       (dired +dirvish +icons)
       ;; electric
       tramp
       undo
       vc

       :term
       eshell
       vterm

       :checkers
       (syntax +childframe)
       spell

       :tools
       debugger
       direnv
       editorconfig
       (eval +overlay)
       (lookup +docsets +dictionary)
       ;; llm
       (lsp +eglot)
       (magit +childframe +forge)
       pdf
       tree-sitter

       :os
       tty

       :lang
       beancount
       (cc +lsp +tree-sitter)
       emacs-lisp
       (gdscript +lsp +tree-sitter)
       janet
       (javascript +lsp +tree-sitter)
       (json +tree-sitter)
       (lua +fennel +lsp +tree-sitter)
       (markdown +tree-sitter)
       (nix +lsp +tree-sitter)
       (org +dragndrop +roam2 +pretty +forge +jupyter)
       (python +lsp +tree-sitter +pyright)
       (rust +lsp +tree-sitter)
       (sh +powershell +fish +lsp)
       ;; (zig +lsp)

       :email
       (mu4e +mbsync +fastmail +org +icons)

       :config
       (default +bindings +smartparens +gnupg))
