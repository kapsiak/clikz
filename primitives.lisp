(in-package :quickdraw)

(defclass primitive () ())

(defclass local-primitive (primitive) ())
(defclass world-primitive (primitive) ())

(defgeneric primitive-space (primitive))
(defmethod primitive-space ((p local-primitive)) :local)
(defmethod primitive-space ((p world-primitive)) :world)

(defgeneric primitive-exact-under (primitive))
(defmethod primitive-exact-under ((p primitive)) :affine)

(defgeneric primitive-slots (primitive))
(defmethod primitive-slots ((p primitive)) nil)
(defgeneric primitive-cull-p (primitive element))
(defgeneric primitive-face-side (primitive element))

(defmacro define-primitive (name (space &key (exact-under :affine)) &body slots)
  (flet ((force-cons (s) (if (consp s) s (list s))))
    `(progn
       (defclass ,name (,(ecase space
                           (:local 'local-primitive)
                           (:world 'world-primitive)))
         ,(loop for s in slots
                collect (destructuring-bind
                            (slot &key initform (reader slot)) (force-cons s)
                          `(,slot :initarg ,(intern (symbol-name slot) :keyword)
                                  :initform ,initform
                                  :reader ,reader))))
       (defmethod primitive-slots ((p ,name)) ',(mapcar (lambda (s) (first (force-cons s))) slots))
       (defmethod primitive-exact-under ((p ,name)) ,exact-under) ',name)))

(defmethod print-object ((prim primitive) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (prim stream :type t)
        (loop for slot in (primitive-slots prim)
              do (format stream " ~a ~s" slot
                         (if (slot-boundp prim slot)
                             (slot-value prim slot)
                             :unbound))))))


(defmethod deep-walk (func (object primitive))
  (let ((new (allocate-instance (class-of object))))
    (dolist (slot (primitive-slots object) new)
      (when (slot-boundp object slot)
        (setf (slot-value new slot)
              (deep-walk func (slot-value object slot)))))))


(defgeneric primitive-centroid (primitive))
(defmethod primitive-centroid ((p primitive)) (vec-4 0 0 0 1))

(defgeneric primitive-sample (primitive &key steps))
(defmethod primitive-sample ((p primitive) &key steps)
  (declare (ignore steps))
  (error "No sampling defined"))

(defgeneric primitive-anchor (primitive key &rest args))
(defmethod primitive-anchor ((p primitive) key &rest args)
  (declare (ignore args))
  (error "No anchor defined"))

(defgeneric primitive-boundary (primitive direction))
(defmethod primitive-boundary ((p primitive) direction)
  (declare (ignore direction))
  (error "No boundary defined"))

(defgeneric primitive-extents (primitive))
(defmethod primitive-extents ((p primitive)))

(defun centroid-of-points (points)
  (let ((n (length points)))
    (if (zerop n)
        (vec-4 0d0 0d0 0d0 1d0)
        (scale-vec (/ 1d0 n) (reduce #'v+ points)))))

(defun centroid-of-polygon (points)
  (when (< (length points) 3)
    (return-from centroid-of-polygon
      (centroid-of-points points)))
  (let* ((edges (loop for p in points
                      for q in (append (cdr points) (list (first points)))
                      collect (cons (xyz p) (xyz q))))
         (nv (reduce #'v+
                     (loop for (p . q) in edges
                           collect (cross-3 p q))))
         (area2 (dot nv nv)))
    (if (zerop area2)
        (centroid-of-points points)
        (loop with s = (vec-3-zeros)
              for (p . q) in edges
              for c = (cross-3 p q)
              for d = (dot c nv)
              for scale = (/ d (* 3.0 area2))
              do (setf s (v+ s (scale-vec scale (v+ p q))))
              finally (return (vec-4 (vec-x s) (vec-y s) (vec-z s) 1d0))))))




