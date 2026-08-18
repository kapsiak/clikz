(in-package :quickdraw)


(defclass element ()
  ((transform :initarg :transform
     :reader element-transform)
   (style :initarg element-style
          :reader element-style)))






