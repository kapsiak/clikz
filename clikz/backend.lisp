(in-package :clikz)

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
    (dolist (element (sort-elements elements))
      (render-primitive backend (element-primitive element) element))))

