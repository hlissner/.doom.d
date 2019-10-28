;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Henrik Lissner"
      user-mail-address "henrik@lissner.net"
      epa-file-encrypt-to user-mail-address

      ;; On-demand code completion. I don't often need it.
      company-idle-delay nil

      ;; lsp-ui-sideline is redundant with eldoc and much more invasive, so
      ;; disable it by default.
      lsp-ui-sideline-enable nil
      lsp-enable-indentation nil
      lsp-enable-on-type-formatting nil
      lsp-enable-symbol-highlighting nil)


;;
;;; UI

(setq doom-font (font-spec :family "Fira Code" :size 12)
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size 13))

(when IS-LINUX
  (font-put doom-font :weight 'semi-light))

;;; Frames/Windows
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;;; Custom modeline
(load! "lisp/modeline")

(custom-theme-set-faces! 'doom-one
  `(org-priority :background ,(doom-color 'bg))
  `(mode-line :foreground ,(doom-color 'blue))
  `(mode-line-buffer-id :foreground ,(doom-color 'fg))
  `(mode-line-success-highlight :background ,(doom-color 'green)))


;;
;;; Keybinds

(map! :leader
      "h L" #'global-keycast-mode
      (:prefix "f"
        "t" #'find-in-dotfiles
        "T" #'browse-dotfiles))


;;
;;; Modules

;;; :completion ivy
(setf (alist-get 'counsel-projectile-find-file ivy-re-builders-alist)
      'ivy--regex-plus)

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
      org-ellipsis " ▼ "
      ;; The standard unicode characters are usually misaligned depending on the
      ;; font. This bugs me. Markdown #-marks for headlines are more elegant.
      org-bullets-bullet-list '("#"))
(after! org
  (add-to-list 'org-modules 'org-habit t))


;;
;;; Frameworks

(def-project-mode! +javascript-screeps-mode
  :match "/screeps\\(?:-ai\\)?/.+$"
  :modes (+javascript-npm-mode)
  :add-hooks (+javascript|init-screeps-mode)
  :on-load (load! "lisp/screeps"))
