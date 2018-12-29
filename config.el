;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

;; (defvar xdg-data (getenv "XDG_DATA_HOME"))
;; (defvar xdg-bin (getenv "XDG_BIN_HOME"))
;; (defvar xdg-cache (getenv "XDG_CACHE_HOME"))
;; (defvar xdg-config (getenv "XDG_CONFIG_HOME"))

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(setq-default
 user-full-name    "Henrik Lissner"
 user-mail-address "henrik@lissner.net"

 ;; doom-variable-pitch-font (font-spec :family "Fira Sans")
 ;; doom-unicode-font (font-spec :family "Input Mono Narrow" :size 12)
 doom-big-font (font-spec :family "Fira Mono" :size 19)

 +workspaces-switch-project-function #'ignore
 +pretty-code-enabled-modes '(emacs-lisp-mode org-mode)
 +format-on-save-enabled-modes '(not emacs-lisp-mode))

;; (setq-hook! 'minibuffer-setup-hook show-trailing-whitespace nil)

(add-to-list 'org-modules 'org-habit t)


;;
;; Host-specific config

(unless (equal (system-name) "proteus")
  ;; I've swapped these keys on my keyboard
  (setq x-super-keysym 'meta
        x-meta-keysym  'super))

(pcase (system-name)
  ("halimede"
   (setq doom-font (font-spec :family "Input Mono Narrow" :size 9)))
  (_
   (setq doom-font (font-spec :family "Input Mono Narrow" :size 12)
         +modeline-height 25)))

(when IS-MAC
  (setq ns-use-thin-smoothing t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-hook 'window-setup-hook #'toggle-frame-maximized))


;;
;; Keybinds

(map! :m "M-j" '+default:multi-next-line
      :m "M-k" '+default:multi-previous-line
      ;; Easier window movement
      :n "C-h" 'evil-window-left
      :n "C-j" 'evil-window-down
      :n "C-k" 'evil-window-up
      :n "C-l" 'evil-window-right
      ;; Simple window/frame navigation/manipulation
      :g "s-w" #'delete-window
      :g "s-W" #'delete-frame
      :g "s-n" #'+default/new-buffer
      :g "s-N" #'make-frame
      ;; Textmate-esque bindings
      :g "s-a" #'mark-whole-buffer
      :n "s-b" #'+default/compile
      :n "s-f" #'swiper
      :g "s-q" (if (daemonp) #'delete-frame #'evil-quit-all)
      ;; Restore OS undo, save, copy, & paste keys (without cua-mode, because
      ;; it imposes some other functionality and overhead we don't need)
      :g "s-z" #'undo
      :g "s-s" #'save-buffer
      :v "s-c" #'copy-region-as-kill
      :g "s-v" #'yank
      ;; textmate-esque newline insertion
      :i [s-return]    #'evil-open-below
      :i [S-s-return]  #'evil-open-above
      ;; textmate-esque deletion
      :i [s-backspace] #'doom/backward-kill-to-bol-and-indent

      (:map evil-treemacs-state-map
        "C-h" 'evil-window-left
        "C-l" 'evil-window-right)

      :leader
      (:prefix "f"
        :desc "Find file in dotfiles" "t" #'+hlissner/find-in-dotfiles
        :desc "Browse dotfiles"       "T" #'+hlissner/browse-dotfiles)
      (:prefix "n"
        :desc "Open mode notes"       "m" #'+hlissner/find-notes-for-major-mode
        :desc "Open project notes"    "p" #'+hlissner/find-notes-for-project))


;;
;; Modules

;; app/rss
(add-hook! 'elfeed-show-mode-hook (text-scale-set 2))

;; emacs/eshell
(after! eshell
  (set-eshell-alias!
   "f"   "find-file $1"
   "l"   "ls -lh"
   "d"   "dired $1"
   "gl"  "(call-interactively 'magit-log-current)"
   "gs"  "magit-status"
   "gc"  "magit-commit"
   "rg"  "rg --color=always $*"))

;; tools/magit
(setq magit-repository-directories '(("~/work" . 2))
      magit-save-repository-buffers nil
      magit-commit-arguments '("--gpg-sign=5F6C0EA160557395")
      magit-rebase-arguments '("--autostash" "--gpg-sign=5F6C0EA160557395")
      magit-pull-arguments   '("--rebase" "--autostash" "--gpg-sign=5F6C0EA160557395"))

;; lang/org
(setq org-directory (expand-file-name "~/work/org/")
      org-agenda-files (list org-directory)
      org-ellipsis " ▼ "

      ;; The standard unicode characters are usually misaligned depending on the
      ;; font. This bugs me. Personally, markdown #-marks for headlines are more
      ;; elegant.
      org-bullets-bullet-list '("#"))


;;
;; Custom

(def-project-mode! +javascript-screeps-mode
  :match "/screeps\\(?:-ai\\)?/.+$"
  :modes (+javascript-npm-mode)
  :add-hooks (+javascript|init-screeps-mode)
  :on-load (load! "+screeps"))
