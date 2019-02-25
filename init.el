;;; ~/.doom.d/init.el -*- lexical-binding: t; -*-

(doom! :feature
       ;;debugger
       eval
       (evil +everywhere)
       (lookup +docsets)
       ;;services
       snippets
       file-templates
       workspaces

       :completion
       company
       ivy
       ;;helm
       ;;ido

       :ui
       doom
       doom-dashboard
       ;;doom-modeline
       ;;doom-quit
       evil-goggles
       ;;fci
       hl-todo
       modeline
       nav-flash
       ;;neotree
       treemacs
       (popup +all +defaults)
       pretty-code
       ;;unicode
       ;;tabbar
       vc-gutter
       vi-tilde-fringe
       window-select

       :editor
       fold              ; (nigh) universal code folding
       format            ; automated prettiness
       ;;lispy             ; vim for lisp, for people who dont like vim
       multiple-cursors  ; editing in many places at once
       ;;parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates

       :emacs
       (dired +ranger)   ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       eshell            ; a consistent, cross-platform shell (WIP)
       imenu             ; an imenu sidebar and searchable code index
       ;;term              ; terminals in Emacs
       vc

       :tools
       ;;ansible
       docker
       editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (flycheck +childframe)
       flyspell
       ;;gist              ; interacting with github gists
       lsp
       ;;macos             ; MacOS-specific commands
       magit             ;
       ;;make              ; run make tasks from Emacs
       password-store    ; password manager for nerds
       pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       ;;rgb               ; creating color strings
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       ;;upload            ; map local to remote projects via ssh/ftp
       ;;wakatime
       vterm

       :lang
       ;;assembly
       (cc +lsp)
       ;;crystal
       ;;clojure
       (csharp +unity)
       ;;common-lisp
       data
       ;;erlang
       ;;elixir
       ;;elm
       emacs-lisp
       ;;ess
       ;;go
       (haskell +intero)
       ;;hy
       ;;(java +lsp)
       (javascript +lsp)
       ;;julia
       latex
       ;;ledger
       lua
       markdown
       ;;nim
       ;;nix
       ;;ocaml
       (org +attach +babel +capture +export +present)
       ;;perl
       ;;php
       ;;plantuml
       ;;purescript
       (python +pyenv +lsp)
       rest
       ;;ruby
       (rust +lsp)
       ;;scala
       sh
       ;;swift
       web

       :app
       ;;notmuch
       ;;crm
       ;;(email +gmail)
       ;;irc
       ;;regex
       ;;(rss +org)
       ;;torrents
       ;;twitter
       ;;(write
       ;; +wordnut
       ;; +langtool)

       :config
       (default +bindings +smartparens))
