;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Henrik Lissner"
      user-mail-address "henrik@lissner.net"

      doom-scratch-initial-major-mode 'lisp-interaction-mode
      doom-theme 'doom-dracula
      treemacs-width 32

      ;; Line numbers are pretty slow all around. The performance boost of
      ;; disabling them outweighs the utility of always keeping them on.
      display-line-numbers-type nil

      ;; IMO, modern editors have trained a bad habit into us all: a burning
      ;; need for completion all the time -- as we type, as we breathe, as we
      ;; pray to the ancient ones -- but how often do you *really* need that
      ;; information? I say rarely. So opt for manual completion:
      company-idle-delay nil

      ;; lsp-ui-sideline is redundant with eldoc and much more invasive, so
      ;; disable it by default.
      lsp-ui-sideline-enable nil
      lsp-enable-symbol-highlighting nil)

(add-load-path! "~/projects/conf/doom-snippets")

(use-package! atomic-chrome
  :after-call focus-out-hook
  :config
  (setq atomic-chrome-default-major-mode 'markdown-mode
        atomic-chrome-buffer-open-style 'frame)
  (atomic-chrome-start-server))


;;
;;; UI

;; "monospace" means use the system default. However, the default is usually two
;; points larger than I'd like, so I specify size 12 here.
(setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))


;;
;;; Keybinds

(map! :n [tab] (cmds! (and (featurep! :editor fold)
                           (save-excursion (end-of-line) (invisible-p (point))))
                      #'+fold/toggle
                      (fboundp 'evil-jump-item)
                      #'evil-jump-item)
      :v [tab] (cmds! (and (bound-and-true-p yas-minor-mode)
                           (or (eq evil-visual-selection 'line)
                               (not (memq (char-after) (list ?\( ?\[ ?\{ ?\} ?\] ?\))))))
                      #'yas-insert-snippet
                      (fboundp 'evil-jump-item)
                      #'evil-jump-item)

      :leader
      "h L" #'global-keycast-mode
      "f t" #'find-in-dotfiles
      "f T" #'browse-dotfiles)


;;
;;; Modules

(after! ivy
  ;; I prefer search matching to be ordered; it's more precise
  (add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus)))

;; I don't need the menu. I know all the shortcuts.
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Silence all that useless output
(setq direnv-always-show-summary nil)

;;; :tools magit
(setq magit-repository-directories '(("~/projects" . 2))
      magit-save-repository-buffers nil
      ;; Don't restore the wconf after quitting magit, it's jarring
      magit-inhibit-save-previous-winconf t
      transient-values '((magit-commit "--gpg-sign=5F6C0EA160557395")
                         (magit-rebase "--autosquash" "--gpg-sign=5F6C0EA160557395")
                         (magit-pull "--rebase" "--gpg-sign=5F6C0EA160557395")))

;;; :lang org
(setq org-directory "~/projects/org/"
      org-archive-location (concat org-directory ".archive/%s::")
      org-roam-directory (concat org-directory "notes/")
      org-journal-encrypt-journal t
      org-ellipsis " ▼ "
      org-superstar-headline-bullets-list '("#"))
(after! org
  (add-to-list 'org-modules 'org-habit t))

;;; :ui doom-dashboard
(setq fancy-splash-image (concat doom-private-dir "splash.png"))

;;; :ui modeline
;; (custom-set-faces!
;;   `(doom-modeline-bar-inactive :background ,(face-background 'mode-line-inactive)))

;; (use-package! keypression
;;   :defer t
;;   :config
;;   (setq ;;keypression-use-child-frame nil
;;         keypression-fade-out-delay 1.0
;;         keypression-frame-justify 'keypression-left-justified
;;         keypression-cast-command-name t
;;         keypression-cast-command-name-format "%s  %s"
;;         keypression-combine-same-keystrokes t
;;         keypression-font-face-attribute '(:width normal :height 200 :weight bold)))


;;
;;; Language customizations

(custom-set-faces!
  `(markdown-code-face :background ,(doom-darken 'bg 0.075)))
