;;; private/hlissner/config.el -*- lexical-binding: t; -*-

(defvar xdg-data (getenv "XDG_DATA_HOME"))
(defvar xdg-bin (getenv "XDG_BIN_HOME"))
(defvar xdg-cache (getenv "XDG_CACHE_HOME"))
(defvar xdg-config (getenv "XDG_CONFIG_HOME"))

(setq +doom-modeline-buffer-file-name-style 'relative-from-project
      show-trailing-whitespace t

      mu4e-maildir        (expand-file-name "mail" xdg-data)
      mu4e-attachment-dir (expand-file-name "attachments" mu4e-maildir))

(add-hook! minibuffer-setup (setq-local show-trailing-whitespace nil))


;;
;; Modules
;;

(after! smartparens
  ;; Auto-close more conservatively and expand braces on RET
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "(" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "[" nil :post-handlers '(("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p)))

;; feature/evil
(after! evil-mc
  ;; Make evil-mc resume its cursors when I switch to insert mode
  (add-hook! 'evil-mc-before-cursors-created
    (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors nil t))
  (add-hook! 'evil-mc-after-cursors-deleted
    (remove-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors t)))

;; completion/helm
(after! helm
  ;; Hide header lines in helm. I don't like them
  (set-face-attribute 'helm-source-header nil :height 0.1))

;; lang/org
(after! org-bullets
  ;; The standard unicode characters are usually misaligned depending on the
  ;; font. This bugs me. Personally, markdown #-marks for headlines are more
  ;; elegant, so we use those.
  (setq org-bullets-bullet-list '("#")))

;; app/irc
(after! circe
  (setq +irc-notifications-watch-strings '("v0" "vnought" "hlissner"))

  (set! :irc "irc.snoonet.org"
    `(:tls t
      :nick "v0"
      :port 6697
      :sasl-username ,(+pass-get-user "irc/snoonet.org")
      :sasl-password ,(+pass-get-secret "irc/snoonet.org")
      :channels (:after-auth "#ynought"))))

;; app/email
(after! mu4e
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

  (set! :email "gmail.com"
    '((mu4e-sent-folder       . "/gmail.com/Sent Mail")
      (mu4e-drafts-folder     . "/gmail.com/Drafts")
      (mu4e-trash-folder      . "/gmail.com/Trash")
      (mu4e-refile-folder     . "/gmail.com/All Mail")
      (smtpmail-smtp-user     . "hlissner")
      (user-mail-address      . "hlissner@gmail.com")
      (mu4e-compose-signature . "---\nHenrik")))

  (set! :email "lissner.net"
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
          :n "x"   #'mu4e-mark-execute-all

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
