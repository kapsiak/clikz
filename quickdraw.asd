(asdf:defsystem :quickdraw
  :author "Charlie Kapsiak <charliekapsiak@gmail.com>"
  :depends-on (:alexandria :cl-svg)
  :components ((:file "quickdraw-package")
               (:file "linalg" :depends-on ("quickdraw-package"))))






