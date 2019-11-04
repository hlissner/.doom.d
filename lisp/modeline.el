;;; /mnt/projects/conf/doom-emacs-private/lisp/modeline.el -*- lexical-binding: t; -*-

;; This is a slimmed down modeline that manipulates `mode-line-format' directly.
;; Its purpose is to be a *significantly* lighter modeline for doom. Doom's
;; modeline has grown so much to meet the demand of users in general that it has
;; become overkill for my needs, so I've returned to the roots.

(defface mode-line-success-highlight '((t (:inherit mode-line-highlight)))
  "TODO")


(defvar mode-line-height 33)

(defvar modeline--redisplayed-p nil)
(defadvice! modeline-recalculate-height-a (&optional _force &rest _ignored)
  :before '(fit-window-to-buffer resize-temp-buffer-window)
  (unless modeline--redisplayed-p
    (setq-local modeline--redisplayed-p t)
    (redisplay t)))

;;; `active'
(defvar selected-window (selected-window))

(defun active ()
  (eq (selected-window) selected-window))

(add-hook! 'pre-redisplay-functions
  (defun set-selected-window (&rest _)
    "Set the variable `selected-window' appropriately."
    (let ((win (selected-window)))
      (unless (minibuffer-window-active-p win)
        (setq selected-window (frame-selected-window))))))

;;; Helpers
(defun make-xpm (color width height)
  "Create an XPM bitmap via COLOR, WIDTH and HEIGHT. Inspired by `powerline''s `pl/make-xpm'."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or color "None")))
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

(defun mode-line-format-icon (icon label &optional face help-echo voffset)
  (propertize (concat (all-the-icons-material
                       icon
                       :face face
                       :height 1.1
                       :v-adjust (or voffset -0.225))
                      (propertize label 'face face))
              'help-echo help-echo))


;;
;;; Segments

;; `mode-line-bar'
(defvar mode-line-bar
  (make-xpm nil 1 (max mode-line-height (frame-char-height))))
(put 'mode-line-bar 'risky-local-variable t)

(defvar mode-line--old-height nil)
(defun adjust-mode-line-height ()
  (unless mode-line--old-height
    (setq mode-line--old-height mode-line-height))
  (let ((default-height mode-line--old-height)
        (scale (or (frame-parameter nil 'font-scale) 0)))
    (if (> scale 0)
        (let* ((font-size (string-to-number
                           (aref (doom--font-name (frame-parameter nil 'font)
                                                  (selected-frame))
                                 xlfd-regexp-pixelsize-subnum)))
               (scale (frame-parameter nil 'font-scale)))
          (setq mode-line-height (+ default-height (* scale doom-font-increment))))
      (setq mode-line-height default-height))
    (setq mode-line-bar (make-xpm nil 1 mode-line-height))))
(add-hook 'doom-change-font-size-hook #'adjust-mode-line-height)

;; `mode-line-matches'
(progn
  (use-package! anzu
    :after-call isearch-mode
    :config
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
    (advice-add #'evil-force-normal-state :before #'anzu--reset-status)
    ;; Fix matches segment mirroring across all buffers
    (mapc #'make-variable-buffer-local
          '(anzu--total-matched anzu--current-position anzu--state
                                anzu--cached-count anzu--cached-positions anzu--last-command
                                anzu--last-isearch-string anzu--overflow-p)))

  (use-package! evil-anzu
    :when (featurep! :editor evil)
    :after-call (evil-ex-start-search evil-ex-start-word-search evil-ex-search-activate-highlight))

  (defun mode-line--anzu ()
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

  (defun mode-line--evil-substitute ()
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

  (defun mode-line--multiple-cursors ()
    "Show the number of multiple cursors."
    (when (bound-and-true-p evil-mc-cursor-list)
      (let ((count (length evil-mc-cursor-list)))
        (when (> count 0)
          (let ((face (cond ((not (active)) 'mode-line-inactive)
                            (evil-mc-frozen 'mode-line-highlight)
                            ('mode-line-success-highlight))))
            (concat (propertize " " 'face face)
                    (all-the-icons-faicon "i-cursor" :face face :v-adjust -0.0575)
                    (propertize " " 'face `(:inherit (variable-pitch ,face)))
                    (propertize (format "%d " count)
                                'face face)))))))

  (defun mode-line--overlay< (a b)
    "Sort overlay A and B."
    (< (overlay-start a) (overlay-start b)))

  (defun mode-line--iedit ()
    "Show the number of iedit regions matches + what match you're on."
    (when (and (bound-and-true-p iedit-mode)
               (bound-and-true-p iedit-occurrences-overlays))
      (propertize
       (let ((this-oc (or (let ((inhibit-message t))
                            (iedit-find-current-occurrence-overlay))
                          (save-excursion
                            (iedit-prev-occurrence)
                            (iedit-find-current-occurrence-overlay))))
             (length (length iedit-occurrences-overlays)))
         (format " %s/%d "
                 (if this-oc
                     (- length
                        (length (memq this-oc (sort (append iedit-occurrences-overlays nil)
                                                    #'mode-line--overlay<)))
                        -1)
                   "-")
                 length))
       'face (if (active) 'mode-line-highlight))))

  (defun mode-line--macro-recording ()
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
      (let ((meta (concat (mode-line--macro-recording)
                          (mode-line--anzu)
                          (mode-line--evil-substitute)
                          (mode-line--iedit)
                          (mode-line--multiple-cursors))))
        (or (and (not (equal meta "")) meta)
            " %I "))))
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
 '((:eval
    (propertize
     (let ((buffer-file-name (buffer-file-name (buffer-base-buffer))))
       (or (when buffer-file-name
             (if-let (project (doom-project-root buffer-file-name))
                 (let ((filename (or buffer-file-truename (file-truename buffer-file-name))))
                   (file-relative-name filename (concat project "..")))))
           "%b"))
     'face (cond ((buffer-modified-p)
                  '(error mode-line-buffer-id))
                 ((active)
                  'mode-line-buffer-id))
     'help-echo buffer-file-name))
   (buffer-read-only (:propertize " RO" face warning))))

;; `mode-line-position'
(setq mode-line-position '("  %l:%C %p  "))

;; `mode-line-checker'
(defun mode-line-checker-update (&optional status)
  "Update flycheck text via STATUS."
  (setq mode-line-checker
        (pcase status
          (`finished
           (if flycheck-current-errors
               (let-alist (flycheck-count-errors flycheck-current-errors)
                 (let ((error (or .error 0))
                       (warning (or .warning 0))
                       (info (or .info 0)))
                   (mode-line-format-icon "do_not_disturb_alt"
                                          (number-to-string (+ error warning info))
                                          (cond ((> error 0)   'error)
                                                ((> warning 0) 'warning)
                                                ('success))
                                          (format "Errors: %d, Warnings: %d, Debug: %d"
                                                  error
                                                  warning
                                                  info))))
             (mode-line-format-icon "check" "" 'success)))
          (`running     (mode-line-format-icon "access_time" "*" 'font-lock-comment-face "Running..."))
          (`errored     (mode-line-format-icon "sim_card_alert" "!" 'error "Errored!"))
          (`interrupted (mode-line-format-icon "pause" "!" 'font-lock-comment-face "Interrupted"))
          (`suspicious  (mode-line-format-icon "priority_high" "!" 'error "Suspicious")))))
(add-hook 'flycheck-status-changed-functions #'mode-line-checker-update)
(add-hook 'flycheck-mode-hook #'mode-line-checker-update)

(defvar-local mode-line-checker nil
  "Displays color-coded error status in the current buffer with pretty
icons.")
(put 'mode-line-checker 'risky-local-variable t)

;; `mode-line-selection-info'
(defsubst doom-modeline-column (pos)
  "Get the column of the position `POS'."
  (save-excursion (goto-char pos)
                  (current-column)))

(defun add-selection-segment ()
  (add-to-list 'mode-line-format-left 'mode-line-selection-info 'append))
(defun remove-selection-segment ()
  (delq! 'mode-line-selection-info mode-line-format-left))

(if (featurep 'evil)
    (progn
      (add-hook 'evil-visual-state-entry-hook #'add-selection-segment)
      (add-hook 'evil-visual-state-exit-hook #'remove-selection-segment))
  (add-hook 'activate-mark-hook #'add-selection-segment)
  (add-hook 'deactivate-mark-hook #'remove-selection-segment))


(defvar mode-line-selection-info
  '(:eval
    (when (or mark-active
              (and (bound-and-true-p evil-local-mode)
                   (eq evil-state 'visual)))
      (cl-destructuring-bind (beg . end)
          (if (boundp 'evil-local-mode)
              (cons evil-visual-beginning evil-visual-end)
            (cons (region-beginning) (region-end)))
        (propertize
         (let ((lines (count-lines beg (min end (point-max)))))
           (concat " "
                   (cond ((or (bound-and-true-p rectangle-mark-mode)
                              (and (bound-and-true-p evil-visual-selection)
                                   (eq 'block evil-visual-selection)))
                          (let ((cols (abs (- (doom-modeline-column end)
                                              (doom-modeline-column beg)))))
                            (format "%dx%dB" lines cols)))
                         ((and (bound-and-true-p evil-visual-selection)
                               (eq evil-visual-selection 'line))
                          (format "%dL" lines))
                         ((> lines 1)
                          (format "%dC %dL" (- end beg) lines))
                         ((format "%dC" (- end beg))))
                   (when (derived-mode-p 'text-mode)
                     (format " %dW" (count-words beg end)))
                   " "))
         'face (if (active) 'success)))))
  "Information about the current selection, such as how many characters and
lines are selected, or the NxM dimensions of a block selection.")
(put 'mode-line-selection-info 'risky-local-variable t)

;; `mode-line-encoding'
(defconst mode-line-encoding
  '(:eval
    (concat (pcase (coding-system-eol-type buffer-file-coding-system)
              (0 " LF ")
              (1 " RLF ")
              (2 " CR "))
            (let ((sys (coding-system-plist buffer-file-coding-system)))
              (if (memq (plist-get sys :category)
                        '(coding-category-undecided coding-category-utf-8))
                  "UTF-8"
                (upcase (symbol-name (plist-get sys :name)))))
            "  ")))
(put 'mode-line-encoding 'risky-local-variable t)


;;
;;; Setup

(defvar-local mode-line-format-left nil)
(put 'mode-line-format-left 'risky-local-variable t)


(defvar-local mode-line-format-right nil)
(put 'mode-line-format-right 'risky-local-variable t)

(setq-default
 mode-line-format-left
 '(""
   mode-line-matches
   " "
   mode-line-buffer-identification
   mode-line-position)

 mode-line-format-right
 `(""
   mode-line-misc-info
   mode-line-modes
   (vc-mode ("  "
             ,(all-the-icons-octicon "git-branch" :v-adjust 0.0)
             vc-mode " "))
   " "
   mode-line-encoding
   (mode-line-checker ("" mode-line-checker "   ")))

 ;;
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

(with-current-buffer "*Messages*"
  (setq mode-line-format (default-value 'mode-line-format)))


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

(defun set-pdf-modeline ()) ; TODO `set-pdf-modeline'


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

(add-hook 'special-mode-hook #'set-special-modeline)
(add-hook 'image-mode-hook #'set-special-modeline)
(add-hook 'circe-mode-hook #'set-special-modeline)
(add-hook 'pdf-tools-enabled-hook #'set-pdf-modeline)
