(asdf:defsystem :quickdraw
  :author "Charlie Kapsiak <charliekapsiak@gmail.com>"
  :depends-on (:alexandria :xmls)
  :serial t
  :components ((:file "quickdraw-package")

               (:file "linalg")
               (:file "linalg-constants")

               (:file "util")
               (:file "color")

               (:file "primitives")
               (:file "elements")
               (:file "resource")
               (:file "core")
               (:file "anchors")

               (:file "drawing/paths")
               (:file "drawing/shapes")
               (:file "drawing/surfaces")
               (:file "drawing/markers")

               (:file "backend")
               (:file "backend-svg")))











