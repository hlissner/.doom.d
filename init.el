;;; $DOOMDIR/init.el -*- lexical-binding: t; -*-

(doom! ;;((sources)
       ;; (flags))

       :completion
       ;; company
       (corfu +dabbrev +icons)
       ;; ivy
       (vertico +icons)

       :ui
       doom
       modeline
       doom-dashboard
       hl-todo
       indent-guides
       ophints
       (popup +all +defaults)
       treemacs
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

       :emacs
       (dired +dirvish +icons)
       electric
       undo
       vc

       :term
       eshell
       vterm

       :checkers
       (syntax +childframe)
       spell

       :tools
       ;;(debugger +lsp)
       direnv
       ;;editorconfig
       (eval +overlay)
       (lookup +docsets +dictionary)
       lsp
       (magit +childframe +forge)
       ;;pdf
       tree-sitter
       ;;upload

       :os
       tty

       :lang
       beancount
       cc
       ;;common-lisp
       emacs-lisp
       (gdscript +lsp)
       ;;haskell
       janet
       ;;(java +meghanada)
       (javascript +lsp)
       ;;julia
       ;;latex
       ;;(lua +fennel)
       markdown
       nix
       (org +dragndrop +roam2 +appear)
       ;;php
       ;;plantuml
       ;;(python +conda)
       ;;rest
       ;;ruby
       (rust +lsp)
       (scheme +guile)
       (sh +powershell)
       web
       yaml
       ;;(zig +lsp)

       :email
       (mu4e +mbsync +fastmail +org +icons)

       :app
       ;;everywhere
       ;;irc

       :config
       ;;literate
       (default +bindings +smartparens +gnupg))
