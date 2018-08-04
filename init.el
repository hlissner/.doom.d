;;; init.el -*- lexical-binding: t; -*-

(doom! :feature
      ;debugger
       eval
       (evil +everywhere)
       (lookup +devdocs +docsets)
      ;services
       snippets
       file-templates
       spellcheck
       (syntax-checker +childframe)
       workspaces

       :completion
       company
      ;ivy
       helm
      ;ido

       :ui
       doom
       doom-dashboard
      ;doom-modeline
      ;doom-quit
       evil-goggles
      ;fci
       hl-todo
       modeline
       nav-flash
      ;neotree
       treemacs
       (popup +all +defaults)
       pretty-code
      ;unicode
      ;tabbar
       vc-gutter
       vi-tilde-fringe
       window-select

       :editor
      ;parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates

       :emacs
       dired             ; making dired pretty [functional]
       ediff             ; comparing files in Emacs
       electric          ; smarter, keyword-based electric-indent
       eshell            ; a consistent, cross-platform shell (WIP)
       hideshow          ; basic code-folding support
       imenu             ; an imenu sidebar and searchable code index
      ;term              ; terminals in Emacs
       vc

       :tools
       editorconfig      ; let someone else argue about tabs vs spaces
      ;gist              ; interacting with github gists
      ;macos             ; MacOS-specific commands
       magit             ;
      ;make              ; run make tasks from Emacs
       password-store    ; password manager for nerds
      ;pdf               ; pdf enhancements
      ;prodigy           ; FIXME managing external services & code builders
      ;rgb               ; creating color strings
      ;tmux              ; an API for interacting with tmux
      ;upload            ; map local to remote projects via ssh/ftp
      ;wakatime

       :lang
      ;assembly
       (cc +irony +rtags)
       crystal
      ;clojure
      ;csharp
      ;common-lisp
       data
      ;erlang
      ;elixir
      ;elm
       emacs-lisp
      ;ess
      ;go
       (haskell +intero)
      ;hy
      ;(java +meghanada)
       javascript
      ;julia
       latex
      ;ledger
       lua
       markdown
      ;nim
      ;nix
      ;ocaml
       (org +attach +babel +capture +export +present)
      ;perl
       php
      ;plantuml
      ;purescript
       python
       rest
       ruby
       rust
      ;scala
       sh
      ;swift
       web

       :app
      ;notmuch
      ;crm
      ;(email +gmail)
      ;irc
      ;regex
      ;rss
      ;torrents
      ;twitter
      ;(write
      ; +wordnut
      ; +langtool)

       :config
       (default +bindings +evil-commands))
