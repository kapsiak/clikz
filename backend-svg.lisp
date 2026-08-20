(in-package :quickdraw)

(defclass svg-backend ()
  ())


(defun point-to-pair (v)
  (format nil "~d ~d" (vec-x v) (vec-y v))) 

(defun points-to-tuple (v1 v2 v3)
  (format nil "~a, ~a, ~a"
          (point-to-pair v1)
          (point-to-pair v2)
          (point-to-pair v3)))


(make-render-for-type
 (svg-backend :path)
 (let ((params (getf-elem element :points))
       (trans (elem->placement-func element))
       (ret nil))
   (do ((i 0 (incf i)))
       ((>= i (length params)))
     (let ((p (nth i params)))
       (cond
         ((eql p :L)
          (progn 
            (push "L" ret)
            (push (point-to-pair (funcall trans (nth (incf i) params))) ret))
          )

         ((eql p :C) (progn
                       (push "C" ret)
                       (push (points-to-tuple
                            (funcall trans (nth (incf i) params))
                            (funcall trans (nth (incf i) params))
                            (funcall trans (nth (incf i) params)))
                           ret)))

         (t (error "Bad path")))))
   (format nil "~{~a~^ ~}" (nreverse ret))))














