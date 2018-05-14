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
       (popup +all +defaults)
       doom
       doom-dashboard
       doom-modeline
      ;doom-quit
       hl-todo
       nav-flash
       evil-goggles
      ;unicode
      ;tabbar
       vi-tilde-fringe
       window-select
       neotree

       :tools
       dired
       editorconfig
       electric-indent
       eshell
      ;gist
       imenu
      ;impatient-mode
      ;macos
       magit
      ;make
      ;password-store
      ;pdf
      ;ranger
       rotate-text
      ;term
      ;tmux
      ;upload

       :lang
      ;assembly
       cc
       crystal
      ;clojure
       csharp
       data
       elixir
      ;elm
       emacs-lisp
      ;ess
      ;go
       (haskell +intero)
      ;hy
      ;(java +meghanada)
       javascript
      ;julia
      ;latex
      ;ledger
       lua
       markdown
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
      ;crm
       (email +gmail)
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
