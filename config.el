;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Henrik Lissner"
      user-mail-address "henrik@lissner.net"

      which-key-idle-delay 2.0
      company-idle-delay nil

      doom-docs-dir "~/work/conf/doom-emacs-docs/docs"

      ;; This functionality is already provided by the lookup module and
      ;; eldoc, on demand, so we disable them by default.
      lsp-ui-sideline-enable nil
      lsp-ui-doc-enable nil)


;;
;;; UI

;;; Fonts
(setq doom-font (font-spec :family "Input Mono Narrow" :size 12)
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size 14))

(pcase (system-name)
  ("halimede"
   (font-put doom-font :size 9)) ; smaller display
  ("triton"
   ;; I've swapped these keys on my keyboard
   (setq x-super-keysym 'meta
         x-meta-keysym  'super)))

(when IS-LINUX
  (font-put doom-font :weight 'semi-light))
(when IS-MAC
  (setq ns-use-thin-smoothing t))

;;; Frames/Windows
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(when IS-MAC
  (add-hook 'window-setup-hook #'toggle-frame-maximized))

;;; Modeline
(defface mode-line-success-highlight '((t (:inherit mode-line-highlight)))
  "TODO")

(load! "lisp/modeline")
(add-hook! 'doom-load-theme-hook
  (set-face-foreground 'mode-line (doom-color 'blue))
  (set-face-foreground 'mode-line-buffer-id (doom-color 'fg))
  (set-face-background 'mode-line-success-highlight (doom-color 'green)))


;;
;;; Keybinds

(map! :m "M-j" #'+hlissner:multi-next-line
      :m "M-k" #'+hlissner:multi-previous-line

      ;; Easier window movement
      :n "C-h" #'evil-window-left
      :n "C-j" #'evil-window-down
      :n "C-k" #'evil-window-up
      :n "C-l" #'evil-window-right

      (:map evil-treemacs-state-map
        "C-h" #'evil-window-left
        "C-l" #'evil-window-right)

      (:when IS-LINUX
        "s-x" #'execute-extended-command
        "s-;" #'eval-expression
        ;; use super for window/frame navigation/manipulation
        "s-w" #'delete-window
        "s-W" #'delete-frame
        "s-n" #'+default/new-buffer
        "s-N" #'make-frame
        "s-q" (if (daemonp) #'delete-frame #'evil-quit-all)
        ;; Restore OS undo, save, copy, & paste keys (without cua-mode, because
        ;; it imposes some other functionality and overhead we don't need)
        "s-z" #'undo
        "s-c" (if (featurep 'evil) #'evil-yank #'copy-region-as-kill)
        "s-v" #'yank
        "s-s" #'save-buffer
        ;; Buffer-local font scaling
        "s-+" #'doom/reset-font-size
        "s-=" #'doom/increase-font-size
        "s--" #'doom/decrease-font-size
        ;; Conventional text-editing keys
        "s-a" #'mark-whole-buffer
        :gi [s-return]    #'+default/newline-below
        :gi [s-S-return]  #'+default/newline-above
        :gi [s-backspace] #'doom/backward-kill-to-bol-and-indent)

      :leader
      (:prefix "f"
        "t" #'+hlissner/find-in-dotfiles
        "T" #'+hlissner/browse-dotfiles)
      (:prefix "n"
        "m" #'+hlissner/find-notes-for-major-mode
        "p" #'+hlissner/find-notes-for-project))


;;
;;; Modules

;;; :ui pretty-code
(setq +pretty-code-enabled-modes '(emacs-lisp-mode org-mode))

;;; :tools magit
(setq magit-repository-directories '(("~/work" . 2))
      magit-save-repository-buffers nil
      transient-values '((magit-commit "--gpg-sign=5F6C0EA160557395")
                         (magit-rebase "--autosquash" "--gpg-sign=5F6C0EA160557395")
                         (magit-pull "--rebase" "--gpg-sign=5F6C0EA160557395")))

;;; :tools vterm
(after! vterm
  (setenv "DOOM_TERM" "1"))

;;; :lang org
(after! org
  (add-to-list 'org-modules 'org-habit t))
(setq org-directory "~/work/org/"
      org-agenda-files (list org-directory)
      org-ellipsis " ▼ "

      ;; The standard unicode characters are usually misaligned depending on the
      ;; font. This bugs me. Markdown #-marks for headlines are more elegant.
      org-bullets-bullet-list '("#"))

;;; :app rss
(add-hook! 'elfeed-show-mode-hook (text-scale-set 2))


;;
;;; Custom

;;; Keycast
(load! "lisp/keycast")


;;
;;; Frameworks

(def-project-mode! +javascript-screeps-mode
  :match "/screeps\\(?:-ai\\)?/.+$"
  :modes (+javascript-npm-mode)
  :add-hooks (+javascript|init-screeps-mode)
  :on-load (load! "lisp/screeps"))
