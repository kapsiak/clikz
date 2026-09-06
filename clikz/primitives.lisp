(in-package :clikz)

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
  "Make it easier to define primitives.
We need to keep track of slots to get automatic delayed resolution."
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
  (error "No sampling defined for primitive ~s" (class-name (class-of p))))



(defgeneric primitive-anchor (primitive key &rest args))
(defmethod primitive-anchor ((p primitive) key &rest args)
  (declare (ignore args))
  (error "No anchor defined"))

(defgeneric primitive-boundary (primitive direction))
(defmethod primitive-boundary ((p primitive) direction)
  (declare (ignore direction))
  (error "No boundary defined"))

(defgeneric primitive-extents (primitive))
(defmethod primitive-extents ((p primitive))
  (primitive-sample p :steps 10))

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




;; Bounding box
(defstruct box
  (min-x 0d0 :type double-float) (max-x 0d0 :type double-float)
  (min-y 0d0 :type double-float) (max-y 0d0 :type double-float)
  (min-z 0d0 :type double-float) (max-z 0d0 :type double-float))

(defun box-width (box) (- (box-max-x box) (box-min-x box)))
(defun box-height (box) (- (box-max-y box) (box-min-y box)))
(defun box-depth (box) (- (box-max-z box) (box-min-z box)))
(defun box-center (box)
  (vec-3  (/ (+ (box-min-x box) (box-max-x box)) 2d0)
          (/ (+ (box-min-y box) (box-max-y box)) 2d0)
          (/ (+ (box-min-z box) (box-max-z box)) 2d0)))

(defun box-dims-as-vec (box)
  (vec-3
   (box-width box)
   (box-height box)
   (box-depth box)))

(defun box-center-to-x (box)
  (vec-3 (/ (box-width box) 2d0) 0d0 0d0))

(defun box-center-to-y (box)
  (vec-3 0d0 (/ (box-height box) 2d0) 0))

(defun box-center-to-z (box)
  (vec-3  0d0 0d0 (/ (box-depth box) 2d0)))

;; Cnvert the extent point into a coarder bounding box
;; Less precise but suitable for general layout 
(defun points->bound-box (points)
  (if (null points)
      (make-box)
      (loop for pt in points
            minimize (vec-x pt) into min-x maximize (vec-x pt) into max-x
            minimize (vec-y pt) into min-y maximize (vec-y pt) into max-y
            minimize (vec-z pt) into min-z maximize (vec-z pt) into max-z
            finally (return (make-box :min-x (to-df min-x)
                                      :max-x (to-df max-x)
                                      :min-y (to-df min-y)
                                      :max-y (to-df max-y)
                                      :min-z (to-df min-z)
                                      :max-z (to-df max-z))))))

(defun box-anchor (box key)
  (flet ((add (&rest funcs)
           (reduce #'v+
                   (mapcar (lambda (f)
                             (if (consp f)
                                 (scale-vec (coerce (car f) 'double-float)
                                            (funcall (second f) box))
                                 (funcall f box)))
                           funcs))))
    (ecase key
      (:center (box-center box))
      (:top (add #'box-center #'box-center-to-y))
      (:bottom (add #'box-center '(-1 box-center-to-y)))
      (:right (add #'box-center #'box-center-to-x))
      (:left (add #'box-center '(-1 box-center-to-x)))
      (:front (add #'box-center #'box-center-to-z))
      (:back (add #'box-center '(-1 box-center-to-z))))))


(defun primtive-bound-box (primitive)
  (points->bound-box (primitive-extents primitive)))
