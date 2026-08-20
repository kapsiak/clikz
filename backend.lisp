(in-package :quickdraw)

(defgeneric init-backend (backend))
(defgeneric finalize-backend (backend))
(defgeneric supports-p (backend feature))
(defgeneric render-element (backend type element))
(defgeneric render-resource (backend resource))
(defgeneric render-math (backend text))

(defmacro make-render-for-type (l &rest body)
  `(defmethod render-element ((backend ,(car l)) (type (eql ,(second l))) element)
     ,@body))


(defun render (backend elements resources)
  (unwind-protect
       (progn
         (init-backend backend)
         (loop for element in elements do
           (render-element backend (element-type element) element))
         (finalize-backend backend))))




