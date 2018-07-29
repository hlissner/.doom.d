;;; private/hlissner/config.el -*- lexical-binding: t; -*-

(defvar xdg-data (getenv "XDG_DATA_HOME"))
;; (defvar xdg-bin (getenv "XDG_BIN_HOME"))
;; (defvar xdg-cache (getenv "XDG_CACHE_HOME"))
(defvar xdg-config (getenv "XDG_CONFIG_HOME"))

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(setq user-full-name    "Henrik Lissner"
      user-mail-address "henrik@lissner.net"

      ;; doom-variable-pitch-font (font-spec :family "Fira Sans")
      ;; doom-unicode-font (font-spec :family "Input Mono Narrow" :size 12)
      doom-big-font (font-spec :family "Fira Mono" :size 19)

      show-trailing-whitespace t
      ;; mu4e
      mu4e-maildir        (expand-file-name "mail" xdg-data)
      mu4e-attachment-dir (expand-file-name "attachments" mu4e-maildir)

      +pretty-code-enabled-modes '(emacs-lisp-mode org-mode))

(setq-hook! 'minibuffer-setup-hook show-trailing-whitespace nil)

;; load heavy packages all sneaky breeky like
(defun auto-require-packages (packages)
  (let* ((reqs (cl-remove-if #'featurep packages))
         (req (pop reqs)))
    (when req
      (require req)
      (when reqs
        (run-with-idle-timer 1 nil #'auto-require-packages reqs)))))

(run-with-idle-timer 1 nil #'auto-require-packages
                     '(calendar find-func format-spec org-macs org-compat
                       org-faces org-entities org-list org-pcomplete org-src
                       org-footnote org-macro ob org org-clock org-agenda
                       org-capture with-editor git-commit package magit))


;;
;; Host-specific config
;;

(pcase (system-name)
  ("halimede"
   (setq ivy-height 12
         ivy-posframe-font (font-spec :family "Input Mono Narrow" :size 12)
         doom-font (font-spec :family "Input Mono Narrow" :size 9)
         ;; I've swapped these keys on my keyboard
         x-super-keysym 'meta
         x-meta-keysym  'super))
  (_
   (setq ivy-posframe-font (font-spec :family "Input Mono Narrow" :size 18)
         doom-font (font-spec :family "Input Mono Narrow" :size 12 :weight 'semi-light))))

(when IS-MAC
  (setq ns-use-thin-smoothing t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  ;; maximize first frame
  (set-frame-parameter nil 'fullscreen 'maximized))


;;
;; Keybindings
;;

(map!
 (:after treemacs
   (:map evil-treemacs-state-map
     "C-h" #'evil-window-left
     "C-l" #'evil-window-right))

 (:leader
   (:prefix "f"
     :desc "Find file in dotfiles" :n "t" #'+hlissner/find-in-dotfiles
     :desc "Browse dotfiles"       :n "T" #'+hlissner/browse-dotfiles)
   (:prefix "n"
     :desc "Browse mode notes"     :n  "m" #'+hlissner/find-notes-for-major-mode
     :desc "Browse project notes"  :n  "p" #'+hlissner/find-notes-for-project)))


;;
;; Modules
;;

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
      magit-commit-arguments '("--gpg-sign=5F6C0EA160557395")
      magit-rebase-arguments '("--autostash" "--gpg-sign=5F6C0EA160557395")
      magit-pull-arguments   '("--rebase" "--autostash" "--gpg-sign=5F6C0EA160557395")
      +magit-hub-features t)

(after! magit
  ;; Add gpg-sign to rebasing by default
  (magit-define-popup-option 'magit-rebase-popup
    ?S "Sign using gpg" "--gpg-sign=" #'magit-read-gpg-secret-key))

;; lang/org
(setq org-directory (expand-file-name "~/work/org/")
      org-agenda-files (list org-directory)
      org-ellipsis " ▼ "

      ;; The standard unicode characters are usually misaligned depending on the
      ;; font. This bugs me. Personally, markdown #-marks for headlines are more
      ;; elegant.
      org-bullets-bullet-list '("#"))

