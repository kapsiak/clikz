(in-package :quickdraw)

(defgeneric init-backend (backend))
(defgeneric finalize-backend (backend))
(defgeneric render-resource (backend resource))
(defgeneric render-math (backend text))

(defgeneric render-primitive (backend primitive element)
  (:documentation "Draw PRIMITIVE, taking transform, style and clip from ELEMENT."))

(defmethod render-primitive ((backend t) (primitive primitive) element)
  (declare (ignore element))
  (warn "Backend ~s does not support ~s; element skipped."
        backend (class-name (class-of primitive))))

(defun element-cull-p (element)
  (primitive-cull-p (element-primitive element) element))

(defun sort-elements (elements)
  (let ((groups (make-hash-table :test #'eq))
        (viewport-order nil)
        (elements-sorted (sort elements #'< :key #'element-index)))
    (dolist (e elements-sorted)
      (let ((vp (element-viewport e)))
        (unless (nth-value 1 (gethash vp groups))
          (push vp viewport-order))
        (push e (gethash vp groups))))
    (loop for vp in (nreverse viewport-order)
          append (stable-sort
                  (remove-if (lambda (e) (primitive-cull-p (element-primitive e) e))
                             (sort (nreverse (gethash vp groups))
                                   #'< :key #'element-index))
                  #'< :key #'element-depth))))

(defun render (backend elements resources)
  (progn
    (init-backend backend)
    (when resources
      (maphash (lambda (k r)
                 (declare (ignore k))
                 (render-resource backend r))
               resources))
    (let (extents transform
          max-x max-y
          min-x min-y)
      (dolist (element (sort-elements elements))
        (render-primitive backend (element-primitive element) element)
        (setf transform (elem->placement-func element))
        (loop for p in (primitive-extents (element-primitive element))
              do (push (funcall transform p) extents)))
      (multiple-value-bind (a b c d)
          (loop for p in extents
                maximizing (vec-x p) into max-x
                minimizing (vec-x p) into min-x
                maximizing (vec-y p) into max-y
                minimizing (vec-y p) into min-y
                finally (return (values min-x min-y max-x max-y)))
        (list a b c d)))))

