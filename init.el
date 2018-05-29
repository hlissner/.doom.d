;;; init.el -*- lexical-binding: t; -*-

(map-put default-frame-alist 'inhibit-double-buffering t)

;; I've swapped these keys on my keyboard
(setq x-super-keysym 'alt
      x-alt-keysym   'meta

      user-mail-address "henrik@lissner.net"
      user-full-name    "Henrik Lissner"

      ;; doom-variable-pitch-font (font-spec :family "Fira Sans")
      ;; doom-unicode-font (font-spec :family "Input Mono Narrow" :size 12)
      doom-big-font (font-spec :family "Fira Mono" :size 19))


(pcase (system-name)
  ((or "proteus" "halimede")
   (setq ivy-height 12
         +doom-modeline-height 24
         ivy-posframe-font (font-spec :family "Input Mono Narrow" :size 12)
         doom-font (font-spec :family "Input Mono Narrow" :size 10)))
  (_
   (setq ivy-posframe-font (font-spec :family "Input Mono Narrow" :size 18)
         doom-font (font-spec :family "Input Mono Narrow" :size 12 :weight 'semi-light))))

;;
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
       version-control
       workspaces

       :completion
       (company +childframe +auto)
       (ivy +childframe)
      ;helm
      ;ido

       :ui
       doom
       doom-dashboard
       doom-modeline
      ;doom-quit
       evil-goggles
       hl-todo
       nav-flash
       neotree
       (popup +all +defaults)
      ;unicode
      ;tabbar
       window-select
       vi-tilde-fringe

       :emacs
       dired             ; making dired pretty [functional]
       ediff             ; comparing files in Emacs
       electric-indent   ; smarter, keyword-based electric-indent
       eshell            ; a consistent, cross-platform shell (WIP)
       imenu             ; an imenu sidebar and searchable code index
      ;term              ; terminals in Emacs

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
       rotate-text       ; cycle region at point between text candidates
      ;tmux              ; an API for interacting with tmux
      ;upload            ; map local to remote projects via ssh/ftp

       :lang
      ;assembly
       cc
      ;crystal
      ;clojure
      ;csharp
       data
      ;erlang
      ;elixir
      ;elm
       emacs-lisp
      ;ess
      ;go
      ;(haskell +intero)
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
      ;ruby
       rust
      ;scala
       sh
      ;swift
       web

       :app
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
