;;; /mnt/projects/conf/doom-emacs-private/lisp/modeline.el -*- lexical-binding: t; -*-

;; This is a slimmed down modeline that manipulates `mode-line-format' directly.
;; Its purpose is to be a *significantly* lighter modeline for doom. Doom's
;; modeline has grown so much to meet the demand of users that it no longer
;; meets my needs, so I went back to the roots.

(defvar mode-line-height 33)

;;; `active'
(defvar selected-window (frame-selected-window)
  "Selected window.")

(defun set-selected-window ()
  "Set the variable `selected-window' appropriately."
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq selected-window (frame-selected-window))
    (force-mode-line-update)))

(defun unset-selected-window ()
  "Unset the variable `selected-window' and update the mode line."
  (setq selected-window nil)
  (force-mode-line-update))

(add-hook 'buffer-list-update-hook #'set-selected-window)
(add-hook 'window-configuration-change-hook 'set-selected-window)
(add-hook 'focus-in-hook 'set-selected-window)
(add-hook 'focus-out-hook 'unset-selected-window)
;; Executes after the window manager requests that the user's events
;; be directed to a different frame.
(defadvice handle-switch-frame (after powerline-handle-switch-frame activate)
  "Call `set-selected-window'."
  (set-selected-window))

(defun active ()
  (eq (selected-window) selected-window))

;;; Helpers
(defun make-xpm (face width height)
  "Create an XPM bitmap via FACE, WIDTH and HEIGHT. Inspired by `powerline''s `pl/make-xpm'."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color "None"))
     (ignore-errors
       (create-image
        (concat
         (format "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
                 (length (car data))
                 (length data)
                 color
                 color)
         (apply #'concat
                (cl-loop with idx = 0
                         with len = (length data)
                         for dl in data
                         do (cl-incf idx)
                         collect
                         (concat "\""
                                 (cl-loop for d in dl
                                          if (= d 0) collect (string-to-char " ")
                                          else collect (string-to-char "."))
                                 (if (eq idx len) "\"};" "\",\n")))))
        'xpm t :ascent 'center)))))


;;
;;; Segments



;;
;;; Segments

;; `mode-line-bar'
(defvar mode-line-bar
  (make-xpm nil 1 (max mode-line-height (frame-char-height))))
(put 'mode-line-bar 'risky-local-variable t)

;; `mode-line-matches'
(progn
  (after! anzu
    ;; anzu and evil-anzu expose current/total state that can be displayed in the
    ;; mode-line.
    (defun doom-modeline-fix-anzu-count (positions here)
      "Calulate anzu counts via POSITIONS and HERE."
      (cl-loop for (start . end) in positions
               collect t into before
               when (and (>= here start) (<= here end))
               return (length before)
               finally return 0))

    (advice-add #'anzu--where-is-here :override #'doom-modeline-fix-anzu-count)

    (setq anzu-cons-mode-line-p nil) ; manage modeline segment ourselves
    ;; Ensure anzu state is cleared when searches & iedit are done
    (add-hook 'isearch-mode-end-hook #'anzu--reset-status t)
    (add-hook 'iedit-mode-end-hook #'anzu--reset-status)
    (advice-add #'evil-force-normal-state :after #'anzu--reset-status)
    ;; Fix matches segment mirroring across all buffers
    (mapc #'make-variable-buffer-local
          '(anzu--total-matched anzu--current-position anzu--state
                                anzu--cached-count anzu--cached-positions anzu--last-command
                                anzu--last-isearch-string anzu--overflow-p)))

  (defsubst modeline--anzu ()
    "Show the match index and total number thereof.
Requires `anzu', also `evil-anzu' if using `evil-mode' for compatibility with
`evil-search'."
    (when (and (bound-and-true-p anzu--state)
               (not (bound-and-true-p iedit-mode)))
      (propertize
       (let ((here anzu--current-position)
             (total anzu--total-matched))
         (cond ((eq anzu--state 'replace-query)
                (format " %d replace " anzu--cached-count))
               ((eq anzu--state 'replace)
                (format " %d/%d " here total))
               (anzu--overflow-p
                (format " %s+ " total))
               (t
                (format " %s/%d " here total))))
       'face (if (active) 'mode-line-highlight))))

  (defsubst modeline--evil-substitute ()
    "Show number of matches for evil-ex substitutions and highlights in real time."
    (when (and (bound-and-true-p evil-local-mode)
               (or (assq 'evil-ex-substitute evil-ex-active-highlights-alist)
                   (assq 'evil-ex-global-match evil-ex-active-highlights-alist)
                   (assq 'evil-ex-buffer-match evil-ex-active-highlights-alist)))
      (propertize
       (let ((range (if evil-ex-range
                        (cons (car evil-ex-range) (cadr evil-ex-range))
                      (cons (line-beginning-position) (line-end-position))))
             (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
         (if pattern
             (format " %s matches " (how-many pattern (car range) (cdr range)))
           " - "))
       'face (if (active) 'mode-line-highlight))))

  (defsubst modeline--multiple-cursors ()
    "Show the number of multiple cursors."
    (when (bound-and-true-p evil-mc-cursor-list)
      (cl-destructuring-bind (count . face)
          (cons (length evil-mc-cursor-list)
                (cond ((not (active)) 'mode-line-inactive)
                      ;; (evil-mc-frozen 'mode-line-highlight)
                      ('mode-line-highlight)))
        (when count
          (concat (propertize " " 'face face)
                  (all-the-icons-faicon "i-cursor" :face face :v-adjust -0.0575)
                  (propertize doom-modeline-vspc 'face `(:inherit (variable-pitch ,face)))
                  (propertize (format "%d " count)
                              'face face))))))

  (defun doom-modeline-themes--overlay-sort (a b)
    "Sort overlay A and B."
    (< (overlay-start a) (overlay-start b)))

  (defsubst modeline--iedit ()
    "Show the number of iedit regions matches + what match you're on."
    (when (and (bound-and-true-p iedit-mode)
               (bound-and-true-p iedit-occurrences-overlays))
      (propertize
       (let ((this-oc (or (let ((inhibit-message t))
                            (iedit-find-current-occurrence-overlay))
                          (progn (iedit-prev-occurrence)
                                 (iedit-find-current-occurrence-overlay))))
             (length (length iedit-occurrences-overlays)))
         (format " %s/%d "
                 (if this-oc
                     (- length
                        (length (memq this-oc (sort (append iedit-occurrences-overlays nil)
                                                    #'doom-modeline-themes--overlay-sort)))
                        -1)
                   "-")
                 length))
       'face (if (active) 'mode-line-highlight))))

  (defsubst modeline--macro-recording ()
    "Display current Emacs or evil macro being recorded."
    (when (and (active)
               (or defining-kbd-macro
                   executing-kbd-macro))
      (let ((sep (propertize " " 'face 'mode-line-highlight)))
        (concat sep
                (propertize (if (bound-and-true-p evil-this-macro)
                                (char-to-string evil-this-macro)
                              "Macro")
                            'face 'mode-line-highlight)
                sep
                (all-the-icons-octicon "triangle-right"
                                       :face 'mode-line-highlight
                                       :v-adjust -0.05)
                sep))))

  (defvar mode-line-matches
    '(:eval
      (let ((meta (concat (modeline--macro-recording)
                          (modeline--anzu)
                          (modeline--evil-substitute)
                          (modeline--iedit)
                          (modeline--multiple-cursors))))
        (or (and (not (equal meta "")) meta)
            " %I ")))
    "TODO")
  (put 'mode-line-matches 'risky-local-variable t))

;; `mode-line-modes'
(setq-default
 mode-line-modes ; remove minor modes
 '(""
   (:propertize mode-name
                face bold
                mouse-face mode-line-highlight)
   mode-line-process
   "%n"
   "%]"
   " ")

 ;; `mode-line-buffer-identification'
 mode-line-buffer-identification ; slightly more informative buffer id
 '(:eval
   (propertize
    "%b"
    'face (if (active)
              (if (buffer-modified-p)
                  '(mode-line-buffer-id error)
                'mode-line-buffer-id)))))

;; `mode-line-position'
(setq mode-line-position '("    %l:%C %p  "))


;; Show indentation style in modeline. I'm not using `doom-modeline-def-segment'
;; to prevent eager macro expansion from loading the package too soon.
(defvar mode-line-indent-style
  (propertize (format "%s%d"
                      (if indent-tabs-mode "SPC" "TAB")
                      tab-width)
              'face (if (active) 'mode-line)
              'mouse-face 'mode-line-highlight
              'help-echo
              (let ((subsegs
                     (list (format "Indentation style: %s (%d wide)"
                                   (if indent-tabs-mode "tabs" "spaces")
                                   tab-width)
                           (cond ((eq doom-inhibit-indent-detection 'editorconfig)
                                  (propertize "✓ Editorconfig applied" 'face 'success))
                                 (doom-inhibit-indent-detection
                                  (propertize "✘ Indentation auto-detection disabled" 'face 'warning))
                                 ((bound-and-true-p dtrt-indent-original-indent)
                                  (propertize (format "✓ Indentation auto-detected (original: %s)"
                                                      dtrt-indent-original-indent)
                                              'face 'success)))
                           (when (bound-and-true-p ws-butler-mode)
                             (propertize "✓ ws-butler active (whitespace cleanup on save)"
                                         'face 'success)))))
                (string-join (delq nil subsegs) "   ")))
  "indent modeline segment")
(put 'mode-line-indent-style 'risky-local-variable t)


;;
;;; Setup

(defvar-local mode-line-format-left
  '(""
    mode-line-matches
    " "
    mode-line-buffer-identification
    mode-line-position))
(put 'mode-line-format-left 'risky-local-variable t)


(defvar-local mode-line-format-right
  `(""
    ;; mode-line-indent-style
    ;; "  "
    mode-line-modes
    (vc-mode ("  "
              ,(all-the-icons-octicon "git-branch" :v-adjust 0.0)
              vc-mode "  "))
    mode-line-misc-info
    mode-line-end-spaces))
(put 'mode-line-format-right 'risky-local-variable t)

(setq-default
 mode-line-format
 '(""
   mode-line-bar
   mode-line-format-left
   (:eval
    (propertize
     " "
     'display
     `((space :align-to (- (+ right right-fringe right-margin)
                           ,(string-width
                             (format-mode-line '("" mode-line-format-right))))))))
   mode-line-format-right))


;;
;;; Other modelines

(defun set-project-modeline ()
  (setq mode-line-format-left
        `(" "
          ,(all-the-icons-octicon
            "file-directory"
            :face 'bold
            :v-adjust -0.05
            :height 1.25)
          (:propertize (" " (:eval (abbreviate-file-name default-directory)))
                       face bold))
        mode-line-format-right
        '("" mode-line-modes)))

(defun set-special-modeline ()
  (setq mode-line-format-left
        '(""
          mode-line-matches
          " "
          mode-line-buffer-identification)
        mode-line-format-right
        '("" mode-line-modes)))

(add-hook 'special-mode-hook #'set-special-modeline)


;;
;;; Bootstrap

(size-indication-mode +1) ; filesize in modeline
(add-hook '+doom-dashboard-mode-hook #'set-project-modeline)

;; Other modes
(defun set-modeline-in-magit ()
  (if (eq major-mode 'magit-status-mode)
      (set-project-modeline)
    (hide-mode-line-mode)))
(add-hook 'magit-mode-hook #'set-modeline-in-magit)
