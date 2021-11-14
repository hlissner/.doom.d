;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Henrik Lissner"
      user-mail-address "contact@henrik.io")

;; When I bring up Doom's scratch buffer with SPC x, it's often to play with
;; elisp or note something down (that isn't worth an entry in my org files). I
;; can do both in `lisp-interaction-mode'.
(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)

;; Line numbers are pretty slow all around. The performance boost of
;; disabling them outweighs the utility of always keeping them on.
(setq display-line-numbers-type nil)

;; IMO, modern editors have trained a bad habit into us all: a burning need for
;; completion all the time -- as we type, as we breathe, as we pray to the
;; ancient ones -- but how often do you *really* need that information? I say
;; rarely. So opt for manual completion:
(setq company-idle-delay nil)

;; Disable invasive lsp-mode features
(setq lsp-ui-sideline-enable nil   ; not anymore useful than flycheck
      lsp-ui-doc-enable nil        ; slow and redundant with K
      lsp-enable-symbol-highlighting nil
      ;; If an LSP server isn't present when I start a prog-mode buffer, you
      ;; don't need to tell me. I know. On some systems I don't care to have a
      ;; whole development environment for some ecosystems.
      +lsp-prompt-to-install-server 'quiet)

;; Implicit /g flag on evil ex substitution, because I less often want the
;; default behavior.
(setq evil-ex-substitute-global t)

(add-load-path! "~/projects/conf/doom-snippets")

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
                        (+ y (/ height 2)))))


;;
;;; UI

(setq doom-theme 'doom-dracula)

;; "monospace" means use the system default. However, the default is usually two
;; points larger than I'd like, so I specify size 12 here.
(setq doom-font (font-spec :family "JetBrainsMono" :size 12 :weight 'light)
      doom-variable-pitch-font (font-spec :family "Noto Serif" :size 13)
      ivy-posframe-font (font-spec :family "JetBrainsMono" :size 15))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))


;;
;;; Keybinds

(map! (:after evil-org
       :map evil-org-mode-map
       :n "gk" (cmd! (if (org-on-heading-p)
                         (org-backward-element)
                       (evil-previous-visual-line)))
       :n "gj" (cmd! (if (org-on-heading-p)
                         (org-forward-element)
                       (evil-next-visual-line))))

      :leader
      "h L" #'global-keycast-mode
      "f t" #'find-in-dotfiles
      "f T" #'browse-dotfiles)


;;
;;; Modules

;;; :editor evil
;; Focus new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;;; :tools magit
(setq magit-repository-directories '(("~/projects" . 2))
      magit-save-repository-buffers nil
      ;; Don't restore the wconf after quitting magit, it's jarring
      magit-inhibit-save-previous-winconf t
      transient-values '((magit-rebase "--autosquash" "--autostash")
                         (magit-pull "--rebase" "--autostash")))

;;; :lang org
(setq org-directory "~/projects/org/"
      org-archive-location (concat org-directory ".archive/%s::")
      org-roam-directory (concat org-directory "notes/")
      org-roam-db-location (concat org-roam-directory ".org-roam.db")
      org-journal-encrypt-journal t
      org-journal-file-format "%Y%m%d.org"
      org-ellipsis " [...] "
      ;; I use g{h,j,k} to traverse headings and TAB to toggle their visibility,
      ;; and leave C-left/C-right to .  I'll do a lot of movement because my
      ;; presentations tend not to be very linear.
      org-tree-slide-skip-outline-level 2)

;;; :ui doom-dashboard
(setq fancy-splash-image (concat doom-private-dir "splash.png"))
;; Hide the menu for as minimalistic a startup screen as possible.
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

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

(define-generic-mode sxhkd-mode
  '(?#)
  '("alt" "Escape" "super" "bspc" "ctrl" "space" "shift") nil
  '("sxhkdrc") nil
  "Simple mode for sxhkdrc files.")
