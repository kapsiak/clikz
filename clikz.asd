(asdf:defsystem :clikz
  :author "Charlie Kapsiak <charliekapsiak@gmail.com>"
  :depends-on (:alexandria :xmls :esrap)
  :in-order-to ((asdf:test-op (asdf:test-op :clikz/tests)))
  :serial t
  :components ((:file "clikz/clikz-package")

               (:file "clikz/linalg")
               (:file "clikz/linalg-constants")

               (:file "clikz/utils/util")
               (:file "clikz/utils/process")
               (:file "clikz/utils/color")

               (:file "clikz/primitives")
               (:file "clikz/elements")
               (:file "clikz/resource")
               (:file "clikz/core")
               (:file "clikz/anchors")

               (:file "clikz/drawing/paths")
               (:file "clikz/drawing/shapes")
               (:file "clikz/drawing/surfaces")
               (:file "clikz/drawing/markers")

               (:file "clikz/drawing/composite")

               (:file "clikz/svg-tools/svg-path")

               (:file "clikz/drawing/math")

               (:file "clikz/backend")
               (:file "clikz/backend-svg")))

