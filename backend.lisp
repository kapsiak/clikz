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

(defun element-cull-p (element)
  (and (eql (element-face-side element) :back)
       (getf (element-params element) :cull t)))


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
                  (remove-if #'element-cull-p
                             (sort (nreverse (gethash vp groups))
                                   #'< :key #'element-index))
                  #'< :key #'element-depth))))

(defun render (backend elements resources)
  (progn
    (init-backend backend)
    (when resources
      (maphash (lambda (k r)
                 (declare (ignore k))
                 (render-resource backend r)) resources))
    (dolist (element (sort-elements elements))
      (if (supports-p backend (element-type element))
          (render-element backend (element-type element) element)
          (warn "Backend ~s does not support ~s; element skipped."
                backend (element-type element))))))
