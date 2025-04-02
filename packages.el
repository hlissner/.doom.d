;;; $DOOMDIR/packages.el

;; :lang
(package! agenix
  :recipe (:host github :repo "t4ccer/agenix.el")
  :pin "70026ee36b86381e26d6e4505ec7836ebbe95e53")


;;; Misc

;; (package! doom-snippets
;;   :recipe (:local-repo "~/projects/doomemacs/snippets"
;;            :files (:defaults "snippets")
;;            :build (:not compile)))

;; (package! doom-themer
;;   :recipe (:local-repo "~/projects/plugins/emacs-doom-themer/"))
;; (package! doom-themes
;;   :recipe (:local-repo "~/projects/plugins/emacs-doom-themes/"
;;            :files ("*.el" "themes/*.el" "extensions/*.el")))
