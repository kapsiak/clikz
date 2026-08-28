(asdf:defsystem :quickdraw
  :author "Charlie Kapsiak <charliekapsiak@gmail.com>"
  :depends-on (:alexandria :xmls)
  :serial t
  :components ((:file "clikz/quickdraw-package")

               (:file "clikz/linalg")
               (:file "clikz/linalg-constants")

               (:file "clikz/util")
               (:file "clikz/color")

               (:file "clikz/primitives")
               (:file "clikz/elements")
               (:file "clikz/resource")
               (:file "clikz/core")
               (:file "clikz/anchors")

               (:file "clikz/drawing/paths")
               (:file "clikz/drawing/shapes")
               (:file "clikz/drawing/surfaces")
               (:file "clikz/drawing/markers")

               (:file "clikz/backend")
               (:file "clikz/backend-svg")))











