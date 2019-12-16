;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Henrik Lissner"
      user-mail-address "henrik@lissner.net"
      epa-file-encrypt-to user-mail-address

      ;; Line numbers are pretty slow all around. The performance boost of
      ;; disabling them outweighs the utility of always keeping them on.
      display-line-numbers-type nil

      ;; On-demand code completion. I don't often need it.
      company-idle-delay nil

      ;; lsp-ui-sideline is redundant with eldoc and much more invasive, so
      ;; disable it by default.
      lsp-ui-sideline-enable nil
      lsp-enable-indentation nil
      lsp-enable-on-type-formatting nil
      lsp-enable-symbol-highlighting nil
      lsp-enable-file-watchers nil

      ;; Disable help mouse-overs for mode-line segments (i.e. :help-echo text).
      ;; They're generally unhelpful and only add confusing visual clutter.
      mode-line-default-help-echo nil
      show-help-function nil)


;;
;;; UI

;; In case we use this config on a system without these fonts, fail silently
(ignore-errors
  (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
        doom-variable-pitch-font (font-spec :family "Noto Sans" :size 13)))

;;; Frames/Windows
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))


;;
;;; Keybinds

(map! :leader
      "h L" #'global-keycast-mode
      "f t" #'find-in-dotfiles
      "f T" #'browse-dotfiles)


;;
;;; Modules

;;; :completion ivy
(add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus))

;;; :ui doom-dashboard
;; (remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

;;; :editor evil
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;;; :tools direnv
(setq direnv-always-show-summary nil)

;;; :tools magit
(setq magit-repository-directories '(("~/projects" . 2))
      magit-save-repository-buffers nil
      ;; Don't restore the wconf after quitting magit
      magit-inhibit-save-previous-winconf t
      transient-values '((magit-commit "--gpg-sign=5F6C0EA160557395")
                         (magit-rebase "--autosquash" "--gpg-sign=5F6C0EA160557395")
                         (magit-pull "--rebase" "--gpg-sign=5F6C0EA160557395")))

;;; :lang org
(setq org-directory "~/projects/org/"
      org-archive-location (concat org-directory "archive/%s::")
      org-ellipsis " ▼ "
      org-bullets-bullet-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷" "☷" "☷" "☷"))
(after! org
  (add-to-list 'org-modules 'org-habit t))
