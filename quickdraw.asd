(asdf:defsystem :quickdraw
  :author "Charlie Kapsiak <charliekapsiak@gmail.com>"
  :depends-on (:alexandria :xmls)
  :serial t
  :components ((:file "quickdraw-package")

               (:file "linalg")
               (:file "linalg-constants")

               (:file "util")

               (:file "elements")
               (:file "resource")
               (:file "core")
               (:file "anchors")
               (:file "primitives")

               (:file "drawing/shapes")
               (:file "drawing/paths")
               (:file "drawing/surfaces")

               (:file "backend")
               (:file "backend-svg")
               (:file "testground")))











