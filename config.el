;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Henrik Lissner"
      user-mail-address "contact@henrik.io")

;; When I bring up Doom's scratch buffer with SPC x, it's often to play with
;; elisp or note something down (that isn't worth an entry in my notes). I can
;; do both in `lisp-interaction-mode'.
(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)


;;
;;; UI

(setq doom-theme 'doom-tomorrow-night
      doom-font (font-spec :family "JetBrainsMono" :size 12)
      doom-variable-pitch-font (font-spec :family "DejaVu Sans" :size 13))

(setq nerd-icons-font-names '("SymbolsNerdFontMono-Regular.ttf"))

;; Line numbers are pretty slow all around. The performance boost of disabling
;; them outweighs the utility of always keeping them on.
(setq display-line-numbers-type nil)

;; Prevents some cases of Emacs flickering.
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))


;;
;;; Keybinds

(map! (:after evil-org
       :map evil-org-mode-map
       :n "gk" (cmds! (org-on-heading-p)
                      #'org-backward-element
                      #'evil-previous-visual-line)
       :n "gj" (cmds! (org-on-heading-p)
                      #'org-forward-element
                      #'evil-next-visual-line))

      :o "o" #'evil-inner-symbol

      :leader
      "h L" #'global-keycast-mode
      (:prefix "f"
       "t" #'find-in-dotfiles
       "T" #'browse-dotfiles)
      (:prefix "n"
       "b" #'org-roam-buffer-toggle
       "d" #'org-roam-dailies-goto-today
       "D" #'org-roam-dailies-goto-date
       "e" (cmd! (find-file (doom-path org-directory "ledger/personal.gpg")))
       "i" #'org-roam-node-insert
       "r" #'org-roam-node-find
       "R" #'org-roam-capture))


;;
;;; Modules

;;; :completion company
;; IMO, modern editors have trained a bad habit into us all: a burning need for
;; completion all the time -- as we type, as we breathe, as we pray to the
;; ancient ones -- but how often do you *really* need that information? I say
;; rarely. So opt for manual completion:
(after! company
  (setq company-idle-delay nil))


;;; :completion corfu
;; IMO, modern editors have trained a bad habit into us all: a burning need for
;; completion all the time -- as we type, as we breathe, as we pray to the
;; ancient ones -- but how often do you *really* need that information? I say
;; rarely. So opt for manual completion:
(after! corfu
  (setq corfu-auto nil))


;;; :ui modeline
;; An evil mode indicator is redundant with cursor shape
(setq doom-modeline-modal nil)


;;; :editor evil
;; Focus new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Implicit /g flag on evil ex substitution, because I use the default behavior
;; less often.
(setq evil-ex-substitute-global t)


;;; :tools lsp
;; Disable invasive lsp-mode features
(after! lsp-mode
  (setq lsp-enable-symbol-highlighting nil
        ;; If an LSP server isn't present when I start a prog-mode buffer, you
        ;; don't need to tell me. I know. On some machines I don't care to have
        ;; a whole development environment for some ecosystems.
        lsp-enable-suggest-server-download nil))
(after! lsp-ui
  (setq lsp-ui-sideline-enable nil  ; no more useful than flycheck
        lsp-ui-doc-enable nil))     ; redundant with K


;;; :tools magit
(setq magit-repository-directories '(("~/projects" . 2))
      magit-save-repository-buffers nil
      ;; Don't restore the wconf after quitting magit, it's jarring
      magit-inhibit-save-previous-winconf t
      evil-collection-magit-want-horizontal-movement t
      magit-openpgp-default-signing-key "FA1FADD9440B688CAA75A057B60957CA074D39A3"
      transient-values '((magit-rebase "--autosquash" "--autostash")
                         (magit-pull "--rebase" "--autostash")
                         (magit-revert "--autostash")))


;;; :lang org
(setq org-directory "~/projects/org/"
      org-roam-directory org-directory
      org-roam-db-location (file-name-concat org-directory ".org-roam.db")
      org-roam-dailies-directory "journal/"
      org-archive-location (file-name-concat org-directory ".archive/%s::")
      org-agenda-files (list org-directory))

(after! org
  (setq org-startup-folded 'show2levels
        org-ellipsis " [...] "
        org-capture-templates
        '(("t" "todo" entry (file+headline "todo.org" "Inbox")
           "* [ ] %?\n%i\n%a"
           :prepend t)
          ("d" "deadline" entry (file+headline "todo.org" "Inbox")
           "* [ ] %?\nDEADLINE: <%(org-read-date)>\n\n%i\n%a"
           :prepend t)
          ("s" "schedule" entry (file+headline "todo.org" "Inbox")
           "* [ ] %?\nSCHEDULED: <%(org-read-date)>\n\n%i\n%a"
           :prepend t)
          ("c" "check out later" entry (file+headline "todo.org" "Check out later")
           "* [ ] %?\n%i\n%a"
           :prepend t)
          ("l" "ledger" plain (file "ledger/personal.gpg")
           "%(+beancount/clone-transaction)"))))

(after! org-roam
  (setq org-roam-capture-templates
        `(("n" "note" plain
           ,(format "#+title: ${title}\n%%[%s/template/note.org]" org-roam-directory)
           :target (file "note/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("r" "thought" plain
           ,(format "#+title: ${title}\n%%[%s/template/thought.org]" org-roam-directory)
           :target (file "thought/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("t" "topic" plain
           ,(format "#+title: ${title}\n%%[%s/template/topic.org]" org-roam-directory)
           :target (file "topic/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("c" "contact" plain
           ,(format "#+title: ${title}\n%%[%s/template/contact.org]" org-roam-directory)
           :target (file "contact/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("p" "project" plain
           ,(format "#+title: ${title}\n%%[%s/template/project.org]" org-roam-directory)
           :target (file "project/%<%Y%m%d>-${slug}.org")
           :unnarrowed t)
          ("i" "invoice" plain
           ,(format "#+title: %%<%%Y%%m%%d>-${title}\n%%[%s/template/invoice.org]" org-roam-directory)
           :target (file "invoice/%<%Y%m%d>-${slug}.org")
           :unnarrowed t)
          ("f" "ref" plain
           ,(format "#+title: ${title}\n%%[%s/template/ref.org]" org-roam-directory)
           :target (file "ref/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("w" "works" plain
           ,(format "#+title: ${title}\n%%[%s/template/works.org]" org-roam-directory)
           :target (file "works/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("s" "secret" plain "#+title: ${title}\n\n"
           :target (file "secret/%<%Y%m%d%H%M%S>-${slug}.org.gpg")
           :unnarrowed t))
        ;; Use human readable dates for dailies titles
        org-roam-dailies-capture-templates
        `(("d" "default" plain ""
           :target (file+head "%<%Y-%m-%d>.org" ,(format "%%[%s/template/journal.org]" org-roam-directory))))))

(after! org-tree-slide
  ;; I use g{h,j,k} to traverse headings and TAB to toggle their visibility, and
  ;; leave C-left/C-right to .  I'll do a lot of movement because my
  ;; presentations tend not to be very linear.
  (setq org-tree-slide-skip-outline-level 2))

(after! org-roam
  ;; Offer completion for #tags and @areas separately from notes.
  (add-to-list 'org-roam-completion-functions #'org-roam-complete-tag-at-point)

  ;; Automatically update the slug in the filename when #+title: has changed.
  (add-hook 'org-roam-find-file-hook #'org-roam-update-slug-on-save-h)

  ;; Make the backlinks buffer easier to peruse by folding leaves by default.
  (add-hook 'org-roam-buffer-postrender-functions #'magit-section-show-level-2)

  ;; List dailies and zettels separately in the backlinks buffer.
  (advice-add #'org-roam-backlinks-section :override #'org-roam-grouped-backlinks-section)

  ;; Open in focused buffer, despite popups
  (advice-add #'org-roam-node-visit :around #'+popup-save-a)

  ;; Make sure tags in vertico are sorted by insertion order, instead of
  ;; arbitrarily (due to the use of group_concat in the underlying SQL query).
  (advice-add #'org-roam-node-list :filter-return #'org-roam-restore-insertion-order-for-tags-a)

  ;; Add ID, Type, Tags, and Aliases to top of backlinks buffer.
  (advice-add #'org-roam-buffer-set-header-line-format :after #'org-roam-add-preamble-a))


;;; :ui doom-dashboard
(setq fancy-splash-image (file-name-concat doom-user-dir "splash.png"))
;; Hide the menu for as minimalistic a startup screen as possible.
(setq +doom-dashboard-functions '(doom-dashboard-widget-banner))


;;; :app everywhere
(after! emacs-everywhere
  ;; Easier to match with a bspwm rule:
  ;;   bspc rule -a 'Emacs:emacs-everywhere' state=floating sticky=on
  (setq emacs-everywhere-frame-name-format "emacs-anywhere")

  ;; The modeline is not useful to me in the popup window. It looks much nicer
  ;; to hide it.
  (remove-hook 'emacs-everywhere-init-hooks #'hide-mode-line-mode)

  ;; Semi-center it over the target window, rather than at the cursor position
  ;; (which could be anywhere).
  (defadvice! center-emacs-everywhere-in-origin-window (frame window-info)
    :override #'emacs-everywhere-set-frame-position
    (cl-destructuring-bind (x y width height)
        (emacs-everywhere-window-geometry window-info)
      (set-frame-position frame
                          (+ x (/ width 2) (- (/ width 2)))
                          (+ y (/ height 2))))))



;;
;;; Language customizations

(use-package! agenix
  :mode ("\\.age\\'" . agenix-mode)
  :config
  (add-to-list 'agenix-key-files "~/.config/ssh/id_ed25519")
  (add-to-list 'agenix-key-files "/etc/ssh/host_ed25519")
  (dolist (file (doom-glob "~/.config/ssh/*/id_ed25519"))
    (add-to-list 'agenix-key-files file)))

(define-generic-mode sxhkd-mode
  '(?#)
  '("alt" "Escape" "super" "bspc" "ctrl" "space" "shift") nil
  '("sxhkdrc") nil
  "Simple mode for sxhkdrc files.")
