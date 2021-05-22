;; -*- no-byte-compile: t; -*-
;;; app/ereader/packages.el

(package! nov :pin "b3c7cc28e95fe25ce7b443e5f49e2e45360944a3")
(package! calibredb
  :recipe (:host github :repo "chenyanming/calibredb.el")
  :pin "a3b04c0c37b1e8ceff2472e21a3579e64e944528")
(package! visual-fill-column :pin "6fa9e7912af412533aec0da8b8f62c227f9f3f54")
(package! mixed-pitch :pin "519e05f74825abf04b7d2e0e38ec040d013a125a")
