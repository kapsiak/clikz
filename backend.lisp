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
  (let ((groups (make-hash-table :test #'eq)))
    (dolist (e elements)
      (push e (gethash (element-viewport e) groups)))
    (loop for vp being each hash-value of groups
          append (stable-sort
                  (remove-if #'element-cull-p vp)
                  #'< :key #'element-depth))))

(defun render (backend elements resources)
  (progn
    (init-backend backend)
    (when resources
      (maphash (lambda (k r) (render-resource backend r)) resources))
    (dolist (element (sort-elements elements))
      (render-element backend (element-type element) element))))
