;;; autoload/org-roam.el -*- lexical-binding: t; -*-

(defvar org-roam-group-order
  '(("Backlinks" t
     (("thought" "💭" org-roam-backlinks-sort-by-date)
      ("note" "📑" org-roam-backlinks-sort-by-date)
      ("journal" "📅" org-roam-backlinks-sort-by-date)))
    ("Relevant" t
     (("secret" "🔒")
      ("topic" "🏷")
      ("project" "🛠")
      ("contact" "🪪")
      ("invoice" "💷" org-roam-backlinks-sort-by-date)))
    ("Works" nil
     (("works" "✏" org-roam-backlinks-sort-by-date)))))

(defun org-roam-backlinks-sort-by-date (a b)
  "Sort backlinks by date (heuristic)."
  (string< (file-name-base (org-roam-node-file (org-roam-backlink-source-node b)))
           (file-name-base (org-roam-node-file (org-roam-backlink-source-node a)))))

(cl-defun org-roam-node-insert-section-with-tags (prefix &key source-node point properties)
  (declare (indent 1))
  (magit-insert-section section (org-roam-node-section)
    (let* ((outline
            (when-let ((outline (plist-get properties :outline))
                       ;; Don't repeat redundant leaves
                       (outline (if (member (car (last outline))
                                            (list (org-roam-node-title org-roam-buffer-current-node)
                                                  (org-roam-node-title source-node)))
                                    (butlast outline)
                                  outline)))
              (mapconcat #'org-link-display-format outline " > ")))
           (title (concat (when prefix
                            (format "%s%s" prefix (propertize " " 'face 'variable-pitch)))
                          (propertize (org-roam-node-title source-node)
                                      'font-lock-face 'org-roam-title)
                          (when outline
                            (format " > %s"
                                    (propertize outline 'font-lock-face 'org-roam-olp)))))
           (tags (org-roam-node-doom-tags source-node))
           (tags (if tags
                     (mapconcat (lambda (tag)
                                  (propertize (concat "#" tag) 'face 'shadow))
                                tags " ")
                   ""))
           (window (get-buffer-window org-roam-buffer)))
      (magit-insert-heading
        (format (format "%%s %%%ds"
                        (- (window-total-width (get-buffer-window org-roam-buffer))
                           (length prefix)
                           (length title)
                           left-margin-width
                           right-margin-width
                           5))  ; who doesn't love magic numbers?
                title
                tags)))
    (oset section node source-node)
    (unless (string-suffix-p ".org.gpg" (org-roam-node-file source-node))
      (magit-insert-section section (org-roam-preview-section)
        (insert (org-roam-fontify-like-in-org-mode
                 (org-roam-preview-get-contents (org-roam-node-file source-node) point))
                "\n")
        (oset section file (org-roam-node-file source-node))
        (oset section point point)
        (insert ?\n)))))

;;;###autoload
(defun org-roam-grouped-backlinks-section (node)
  "The backlinks section for NODE."
  (let ((groups (seq-group-by
                 (lambda (backlink)
                   (org-roam-node-doom-type
                    (org-roam-backlink-source-node backlink)))
                 (org-roam-backlinks-get node)))
        (heading (make-marker)))
    (pcase-dolist (`(,title ,always-show ,group) org-roam-group-order)
      (magit-insert-section (gensym "roam")
        (set-marker heading (point))
        (pcase-dolist (`(,key ,prefix ,sortfn) group)
          (let ((backlinks (cdr (assoc key groups))))
            (when (and (or backlinks always-show)
                       (marker-position heading))
              (goto-char heading)
              (magit-insert-heading (format "%s:" title))
              (set-marker heading nil))
            (dolist (backlink (seq-sort (or sortfn #'org-roam-backlinks-sort) backlinks))
              (org-roam-node-insert-section-with-tags prefix
                :source-node (org-roam-backlink-source-node backlink)
                :point (org-roam-backlink-point backlink)
                :properties (org-roam-backlink-properties backlink))))))
      (set-marker heading nil)
      (insert ?\n))))

;;;###autoload
(defun org-roam-complete-tag-at-point ()
  "Complete #tags at point (tags are notes in {org-roam-directory}/tags/*.org)."
  (when (or (save-match-data
              (org-in-regexp "\\(?:^\\|[ \t]\\)#[a-zA-Z/_-]*"))
            (save-excursion
              (goto-char (line-beginning-position))
              (looking-at-p "#\\+filetags:")))
    (cl-destructuring-bind (beg . end)
        (or (bounds-of-thing-at-point 'symbol)
            (cons (point) (point)))
      (list beg end
            (append org-file-tags
                    (cl-loop for (tag)
                             in (org-roam-db-query
                                 [:select alias :from aliases :where (like alias $s1)]
                                 (concat "#" (buffer-substring-no-properties beg end) "%"))
                             if (string-prefix-p "#" tag)
                             collect (substring tag 1)))
            :exit-function
            (lambda (str _status)
              (delete-char (- (length str)))
              (insert str))
            :exclusive 'no))))


;;
;;; Advice

;;;###autoload
(defun org-roam-restore-insertion-order-for-tags-a (nodes)
  "`org-roam-node-list' returns a list of `org-roam-node's whose tags property
are arbitrarily sorted, due to the use of group_concat in the sqlite query used
to generate it."
  (mapcar (lambda (node)
            (oset node tags
                  (ignore-errors
                    (split-string (cdr (assoc "ALLTAGS" (oref node properties)))
                                  ":" t)))
            node)
          nodes))

;;;###autoload
(defun org-roam-add-preamble-a (&rest _)
  (let ((node org-roam-buffer-current-node))
    (insert
     (format "%-10s %s\n" (propertize "ID:" 'face 'bold)
             (org-roam-node-id node))
     (format "%-10s %s\n" (propertize "Type:" 'face 'bold)
             (or (org-roam-node-doom-type node) "-"))
     (format "%-10s %s\n" (propertize "Tags:" 'face 'bold)
             (if-let (tags (org-roam-node-tags node))
                 (mapconcat (lambda (tag)
                              (propertize (concat "#" tag) 'face 'org-tag))
                            tags " ")
               "-"))
     (format "%-10s %s\n" (propertize "Aliases:" 'face 'bold)
             (if-let (aliases (org-roam-node-aliases node))
                 (string-join aliases ", ")
               "-"))
     ?\n)))


;;
;;; Hooks

(defvar org-roam-old-slug nil)
;;;###autoload
(defun org-roam-update-slug-on-save-h ()
  "Set up auto-updating for the current node's filename.

Calls `org-roam-update-slug-h' on `after-save-hook'."
  (setq-local org-roam-old-slug (ignore-errors (org-roam-node-slug (org-roam-node-at-point))))
  (add-hook 'after-save-hook #'org-roam-update-slug-h
            'append 'local))

(defun org-roam-update-slug-h ()
  "Rename the current file if #+title has changed.

Will ask for confirmation if the new filename already exists."
  (when (org-roam-buffer-p)
    (when-let* ((node (org-roam-node-at-point))
                (new-slug (org-roam-node-slug node))
                (old-slug org-roam-old-slug)
                (old-slug-re (concat "/[^/]*\\(" (regexp-quote old-slug) "\\)[^/]*\\.org$"))
                (file-name (org-roam-node-file node))
                ((not (equal old-slug new-slug)))
                ((string-match-p old-slug-re file-name)))
      (setq org-roam-old-slug new-slug)
      (condition-case _
          (let ((new-file-name
                 (replace-regexp-in-string
                  old-slug-re (regexp-quote new-slug)
                  file-name nil nil 1)))
            (message "Updating slug in filename (%S -> %S)" old-slug new-slug)
            (rename-file file-name new-file-name 1)
            (set-visited-file-name new-file-name t t)
            (org-roam-db-autosync--setup-file-h))
        (error
         (setq org-roam-old-slug old-slug))))))
