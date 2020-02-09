;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Henrik Lissner"
      user-mail-address "henrik@lissner.net"

      doom-scratch-buffer-major-mode 'org-mode
      doom-theme 'doom-dracula
      treemacs-width 32

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

(add-load-path! "~/projects/conf/doom-snippets")


;;
;;; UI

;; In case we use this config on a system without these fonts, fail silently
(setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 13))

;;; Frames/Windows
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))


;;
;;; Keybinds

(map! "s-s" #'save-buffer
      :leader
      "h L" #'global-keycast-mode
      "f t" #'find-in-dotfiles
      "f T" #'browse-dotfiles)


;;
;;; Modules

;;; :completion ivy
(add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus))

;;; :ui doom-dashboard
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

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

;;; :ui modeline
(custom-set-faces!
  `(doom-modeline-bar-inactive :background ,(face-background 'mode-line-inactive)))


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

;; (add-hook! 'lsp-ui-mode-hook
;;   (when (eq major-mode 'js2-mode)
;;     (flycheck-select-checker 'javascript-standard)))

;; (after! org
;;   (load! "test"))

;; (general-auto-unbind-keys)
;; (map! :after org
;;       :map org-mode-map
;;       :localleader
;;       "'" #'org-edit-special
;;       "e" #'org-export-dispatch
;;       "a" #'org-agenda
;;       "p" #'org-priority
;;       ;; More cycling options (timestamps, headlines, items, properties)
;;       "L" #'org-shiftright
;;       "H" #'org-shiftleft
;;       "J" #'org-shiftdown
;;       "K" #'org-shiftup

;;       ;; Enable latex preview
;;       "lp" #'org-latex-preview

;;       "*" #'org-ctrl-c-star
;;       "-" #'org-ctrl-c-minus
;;       "#" #'org-update-statistics-cookies
;;       "RET"   #'org-ctrl-c-ret
;;       "M-RET" #'org-meta-return

;;       ;; attachments
;;       "A" #'org-attach

;;       ;; Change between TODO sets
;;       "C-S-l" #'org-shiftcontrolright
;;       "C-S-h" #'org-shiftcontrolleft
;;       "C-S-j" #'org-shiftcontroldown
;;       "C-S-k" #'org-shiftcontrolup

;;       ;; Clock
;;       (:prefix ("C" . "Clock")
;;         "c" #'org-clock-cancel
;;         "d" #'org-clock-display
;;         "e" #'org-evaluate-time-range
;;         "g" #'org-clock-goto
;;         "i" #'org-clock-in
;;         "I" #'org-clock-in-last
;;         "o" #'org-clock-out
;;         "R" #'org-clock-report
;;         "r" #'org-resolve-clocks)

;;       "d" nil
;;       (:prefix ("d" . "Deadline")
;;         "d" #'org-deadline
;;         "s" #'org-schedule
;;         "t" #'org-time-stamp
;;         "T" #'org-time-stamp-inactive)


;;       (:prefix ("f" . "Feed")
;;         "i" #'org-feed-goto-inbox
;;         "u" #'org-feed-update-all)

;;       (:prefix ("T" . "Toggle")
;;         "c" #'org-toggle-checkbox
;;         "e" #'org-toggle-pretty-entities
;;         "i" #'org-toggle-inline-images
;;         "l" #'org-toggle-link-display
;;         "t" #'org-show-todo-tree
;;         "T" #'org-todo
;;         "V" #'space-doc-mode
;;         "x" #'org-toggle-latex-fragment)

;;       (:prefix ("s" . "Subtree")
;;         "sa" #'org-toggle-archive-tag
;;         "A" #'org-archive-subtree
;;         "b" #'org-tree-to-indirect-buffer
;;         "d" #'org-cut-subtree
;;         "h" #'org-promote-subtree
;;         "j" #'org-move-subtree-down
;;         "k" #'org-move-subtree-up
;;         "l" #'org-demote-subtree
;;         "n" #'org-narrow-to-subtree
;;         "N" #'widen
;;         "r" #'org-refile
;;         "s" #'org-sparse-tree
;;         "S" #'org-sort)

;;       ;; tables
;;       (:prefix ("t" . "Tables")
;;         "a" #'org-table-align
;;         "b" #'org-table-blank-field
;;         "c" #'org-table-convert
;;         "dc" #'org-table-delete-column
;;         "dr" #'org-table-kill-row
;;         "e" #'org-table-eval-formula
;;         "E" #'org-table-export
;;         "f" #'org-table-field-info
;;         "h" #'org-table-previous-field
;;         "H" #'org-table-move-column-left
;;         "w" #'org-table-wrap-region

;;         ;; Table-insert
;;         (:prefix ("i" . "Insert")
;;           "c" #'org-table-insert-column
;;           "h" #'org-table-insert-hline
;;           "H" #'org-table-hline-and-move
;;           "r" #'org-table-insert-row
;;           "I" #'org-table-import
;;           "j" #'org-table-next-row
;;           "J" #'org-table-move-row-down
;;           "K" #'org-table-move-row-up
;;           "l" #'org-table-next-field
;;           "L" #'org-table-move-column-right
;;           "n" #'org-table-create
;;           "N" #'org-table-create-with-table.el
;;           "r" #'org-table-recalculate
;;           "s" #'org-table-sort-lines)

;;         ;; Table-toggle
;;         (:prefix ("T" . "Toggle")
;;           "tf" #'org-table-toggle-formula-debugger
;;           "to" #'org-table-toggle-coordinate-overlays))

;;       ;; Source blocks / org-babel
;;       (:prefix ("b" . "SRC")
;;         "p"     #'org-babel-previous-src-block
;;         "n"     #'org-babel-next-src-block
;;         "e"     #'org-babel-execute-maybe
;;         "o"     #'org-babel-open-src-block-result
;;         "v"     #'org-babel-expand-src-block
;;         "u"     #'org-babel-goto-src-block-head
;;         "g"     #'org-babel-goto-named-src-block
;;         "r"     #'org-babel-goto-named-result
;;         "b"     #'org-babel-execute-buffer
;;         "s"     #'org-babel-execute-subtree
;;         "d"     #'org-babel-demarcate-block
;;         "t"     #'org-babel-tangle
;;         "f"     #'org-babel-tangle-file
;;         "c"     #'org-babel-check-src-block
;;         "j"     #'org-babel-insert-header-arg
;;         "l"     #'org-babel-load-in-session
;;         "i"     #'org-babel-lob-ingest
;;         "I"     #'org-babel-view-src-block-info
;;         "z"     #'org-babel-switch-to-session
;;         "Z"     #'org-babel-switch-to-session-with-code
;;         "a"     #'org-babel-sha1-hash
;;         "x"     #'org-babel-do-key-sequence-in-edit-buffer)


;;       (:prefix ("i" . "Insert")
;;         ;; insertion
;;         "b" #'org-insert-structure-template
;;         "d" #'org-insert-drawer
;;         "e" #'org-set-effort
;;         "f" #'org-footnote-new
;;         "h" #'org-insert-heading
;;         "H" #'org-insert-heading-after-current
;;         "i" #'org-insert-item
;;         "l" #'org-insert-link
;;         "n" #'org-add-note
;;         "p" #'org-set-property
;;         "s" #'org-insert-subheading
;;         "t" #'org-set-tags-command))
