(asdf:defsystem :quickdraw
  :author "Charlie Kapsiak <charliekapsiak@gmail.com>"
  :depends-on (:alexandria :xmls)
  :serial t
  :components ((:file "quickdraw-package")

               (:file "linalg")
               (:file "linalg-constants")

               (:file "util")

               (:file "core")
               (:file "anchors")

               (:file "drawing/paths")

               (:file "backend")
               (:file "backend-svg")
               (:file "testground")))











