(asdf:defsystem :quickdraw
  :author "Charlie Kapsiak <charliekapsiak@gmail.com>"
  :depends-on (:alexandria :xmls)
  :serial t
  :components ((:file "quickdraw-package")
               (:file "linalg")
               (:file "util")
               (:file "core")))









