;;; private/hlissner/config.el -*- lexical-binding: t; -*-

(defvar xdg-data (getenv "XDG_DATA_HOME"))
;; (defvar xdg-bin (getenv "XDG_BIN_HOME"))
;; (defvar xdg-cache (getenv "XDG_CACHE_HOME"))
(defvar xdg-config (getenv "XDG_CONFIG_HOME"))

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(setq user-full-name    "Henrik Lissner"
      user-mail-address "henrik@lissner.net"

      ;; I've swapped these keys on my keyboard
      x-super-keysym 'alt
      x-alt-keysym   'meta

      ;; doom-variable-pitch-font (font-spec :family "Fira Sans")
      ;; doom-unicode-font (font-spec :family "Input Mono Narrow" :size 12)
      doom-big-font (font-spec :family "Fira Mono" :size 19)

      +doom-modeline-buffer-file-name-style 'relative-from-project
      show-trailing-whitespace t
      ;; mu4e
      mu4e-maildir        (expand-file-name "mail" xdg-data)
      mu4e-attachment-dir (expand-file-name "attachments" mu4e-maildir)

      +pretty-code-enabled-modes '(emacs-lisp-mode org-mode))

(setq-hook! 'minibuffer-setup-hook show-trailing-whitespace nil)


;;
;; Host-specific config
;;

(pcase (system-name)
  ((or "proteus" "halimede")
   (setq ivy-height 12
         +doom-modeline-height 23
         ivy-posframe-font (font-spec :family "Input Mono Narrow" :size 12)
         doom-font (font-spec :family "Input Mono Narrow" :size 9)))
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

;; emacs/eshell
(after! eshell
  (set-eshell-alias!
   "q"   "quit-and-close"
   "l"   "ls -l"
   "la"  "ls -la"
   "f"   "find-file $1"
   "d"   "dired $1"
   "gl"  "(call-interactively 'magit-log-current)"
   "gs"  "magit-status"
   "gc"  "magit-commit"
   "rg"  "rg --color=always $*"))

;; tools/magit
(setq magit-repository-directories '(("~/work" . 2))
      magit-commit-arguments '("--gpg-sign=5F6C0EA160557395")
      magit-rebase-arguments '("--autostash" "--gpg-sign=5F6C0EA160557395")
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

;; app/email
(after! mu4e
  (setq mu4e-get-mail-command (format "mbsync -c '%s/mbsync/config' -a" xdg-config))

  (setq mu4e-bookmarks
        `(("\\\\Inbox" "Inbox" ?i)
          ("\\\\Draft" "Drafts" ?d)
          ("flag:unread AND \\\\Inbox" "Unread messages" ?u)
          ("flag:flagged" "Starred messages" ?s)
          ("date:today..now" "Today's messages" ?t)
          ("date:7d..now" "Last 7 days" ?w)
          ("mime:image/*" "Messages with images" ?p)))

  (setq smtpmail-stream-type 'starttls
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587)

  (set-email-account! "gmail.com"
    '((mu4e-sent-folder       . "/gmail.com/Sent Mail")
      (mu4e-drafts-folder     . "/gmail.com/Drafts")
      (mu4e-trash-folder      . "/gmail.com/Trash")
      (mu4e-refile-folder     . "/gmail.com/All Mail")
      (smtpmail-smtp-user     . "hlissner")
      (user-mail-address      . "hlissner@gmail.com")
      (mu4e-compose-signature . "---\nHenrik")))

  (set-email-account! "lissner.net"
    '((mu4e-sent-folder       . "/lissner.net/Sent Mail")
      (mu4e-drafts-folder     . "/lissner.net/Drafts")
      (mu4e-trash-folder      . "/lissner.net/Trash")
      (mu4e-refile-folder     . "/lissner.net/All Mail")
      (smtpmail-smtp-user     . "henrik@lissner.net")
      (user-mail-address      . "henrik@lissner.net")
      (mu4e-compose-signature . "---\nHenrik Lissner"))
    t)

  ;; an evil-esque keybinding scheme for mu4e
  (setq mu4e-view-mode-map (make-sparse-keymap)
        ;; mu4e-compose-mode-map (make-sparse-keymap)
        mu4e-headers-mode-map (make-sparse-keymap)
        mu4e-main-mode-map (make-sparse-keymap))

  (map! (:map (mu4e-main-mode-map mu4e-view-mode-map)
          :leader
          :n "," #'mu4e-context-switch
          :n "." #'mu4e-headers-search-bookmark
          :n ">" #'mu4e-headers-search-bookmark-edit
          :n "/" #'mu4e~headers-jump-to-maildir)

        (:map (mu4e-headers-mode-map mu4e-view-mode-map)
          :localleader
          :n "f" #'mu4e-compose-forward
          :n "r" #'mu4e-compose-reply
          :n "c" #'mu4e-compose-new
          :n "e" #'mu4e-compose-edit)

        (:map mu4e-main-mode-map
          :n "q"   #'mu4e-quit
          :n "u"   #'mu4e-update-index
          :n "U"   #'mu4e-update-mail-and-index
          :n "J"   #'mu4e~headers-jump-to-maildir
          :n "c"   #'+email/compose
          :n "b"   #'mu4e-headers-search-bookmark)

        (:map mu4e-headers-mode-map
          :n "q"   #'mu4e~headers-quit-buffer
          :n "r"   #'mu4e-compose-reply
          :n "c"   #'mu4e-compose-edit
          :n "s"   #'mu4e-headers-search-edit
          :n "S"   #'mu4e-headers-search-narrow
          :n "RET" #'mu4e-headers-view-message
          :n "u"   #'mu4e-headers-mark-for-unmark
          :n "U"   #'mu4e-mark-unmark-all
          :n "v"   #'evil-visual-line
          :nv "d"  #'+email/mark
          :nv "="  #'+email/mark
          :nv "-"  #'+email/mark
          :nv "+"  #'+email/mark
          :nv "!"  #'+email/mark
          :nv "?"  #'+email/mark
          :nv "r"  #'+email/mark
          :nv "m"  #'+email/mark
          :n  "x"  #'mu4e-mark-execute-all

          :n "]]"  #'mu4e-headers-next-unread
          :n "[["  #'mu4e-headers-prev-unread

          (:localleader
            :n "s" 'mu4e-headers-change-sorting
            :n "t" 'mu4e-headers-toggle-threading
            :n "r" 'mu4e-headers-toggle-include-related

            :n "%" #'mu4e-headers-mark-pattern
            :n "t" #'mu4e-headers-mark-subthread
            :n "T" #'mu4e-headers-mark-thread))

        (:map mu4e-view-mode-map
          :n "q" #'mu4e~view-quit-buffer
          :n "r" #'mu4e-compose-reply
          :n "c" #'mu4e-compose-edit
          :n "o" #'ace-link-mu4e

          :n "<M-Left>"  #'mu4e-view-headers-prev
          :n "<M-Right>" #'mu4e-view-headers-next
          :n "[m" #'mu4e-view-headers-prev
          :n "]m" #'mu4e-view-headers-next
          :n "[u" #'mu4e-view-headers-prev-unread
          :n "]u" #'mu4e-view-headers-next-unread

          (:localleader
            :n "%" #'mu4e-view-mark-pattern
            :n "t" #'mu4e-view-mark-subthread
            :n "T" #'mu4e-view-mark-thread

            :n "d" #'mu4e-view-mark-for-trash
            :n "r" #'mu4e-view-mark-for-refile
            :n "m" #'mu4e-view-mark-for-move))

        (:map mu4e~update-mail-mode-map
          :n "q" #'mu4e-interrupt-update-mail)))
