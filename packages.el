;;; ~/.config/doom/packages.el

(package! forge :disable t)

(package! doom-snippets
  :recipe (:local-repo "~/projects/conf/doom-snippets"
           :files ("*.el" "snippets")))

;; (package! doom-themer
;;   :recipe (:local-repo "~/projects/plugins/emacs-doom-themer/"))
;; (package! doom-themes
;;   :recipe (:local-repo "~/projects/plugins/emacs-doom-themes/"
;;            :files ("*.el" "themes/*.el" "extensions/*.el")))

(package! atomic-chrome)
