(in-package :quickdraw)

(defclass svg-backend ()
  ((elements :accessor svg-backend-elements :initform nil)))


(defun svg-add (backend element)
  (push element (svg-backend-elements backend)))


(defun style->list (style)
  (loop for (k v) on style
        by #'cddr
        collect (list (string-downcase (string k)) v)))


(defun matrix->svg-transform (mat3)
  (format nil "matrix(~f ~f ~f ~f ~f ~f)"
          (aref mat3 0 0) (aref mat3 1 0)
          (aref mat3 0 1) (aref mat3 1 1)
          (aref mat3 0 2) (aref mat3 1 2)))


(defun svg-to-string (backend)
  (with-output-to-string (out)
    (xmls:write-xml
     (xmls:nodelist->node 
      `("svg" (("width" ,(format nil "~d" (svg-backend-width backend)))
               ("height" ,(format nil "~d" (svg-backend-height backend)))
               ("xmlns" "http://www.w3.org/2000/svg"))
              ,@(when (svg-backend-defs backend)
                  `(("defs" nil ,@(nreverse (svg-backend-defs backend)))))
              ,@(nreverse (svg-backend-elements backend))))
     out :indent 2)))

(defun point-to-pair (v)
  (format nil "~F ~F" (vec-x v) (vec-y v))) 

(defun points-to-tuple (v1 v2 v3)
  (format nil "~a, ~a, ~a"
          (point-to-pair v1)
          (point-to-pair v2)
          (point-to-pair v3)))

(defmethod init-backend ((backend svg-backend)))

(defmethod render-element ((backend svg-backend) (type (eql :path)) element)
  (let ((params (getf-elem element :points))
        (trans (elem->placement-func element))
        (ret nil))
    (do ((i 0 (incf i)))
        ((>= i (length params)))
      (let ((p (nth i params)))
        (cond
          ((eql p :L)
           (progn 
             (push
              (format nil "L ~a"
                      (point-to-pair (funcall trans (nth (incf i) params))))
              ret)))
          ((eql p :M)
           (progn 
             (push
              (format nil "M ~a"
                      (point-to-pair (funcall trans (nth (incf i) params))))
              ret)))

          ((eql p :C) (progn
                        (format nil "C ~a"
                                (push (points-to-tuple
                                       (funcall trans (nth (incf i) params))
                                       (funcall trans (nth (incf i) params))
                                       (funcall trans (nth (incf i) params)))
                                      ret))))

          (t (error "Bad path")))))
    (svg-add backend
             (list "path"  (append
                            (list (list "d" (format nil "~{~a~^ ~}" (nreverse ret))))
                            (style->list (element-style element)))))))















