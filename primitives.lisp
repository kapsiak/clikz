(in-package :quickdraw)

(defparameter +primitive-types+ (list
                                 :rect ; :x :y :w :h
                                 :circle ; :r :x ;y
                                 :ellipse ; :rx :ry :x :y
                                 :path ; :points :closed
                                 :polyline ; :points :closed
                                 :segment ; :start :end
                                 :label ; :text :anchor 
                                 :face ; :points :normal
                                 ))


(defgeneric primitive-centroid (kind params))

(defmethod primitive-centroid ((kind t) params)
  (declare (ignore kind params))
  (vec-4 0 0 0 1))

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

(defun collect-points (x)
  (cond ((typep x '(simple-array double-float (*))) (list x))
        ((listp x) (mapcan #'collect-points x))
        (t nil)))

(defmethod primitive-centroid ((kind (eql :segment)) params)
  (centroid-of-points (list (getf params :start) (getf params :end))))

(defmethod primitive-centroid ((kind (eql :rect)) params)
  (declare (ignore params))
  (vec-4 0d0 0d0 0d0 1d0))

(defmethod primitive-centroid ((kind (eql :circle)) params)
  (declare (ignore params))
  (vec-4 0d0 0d0 0d0 1d0))

(defmethod primitive-centroid ((kind (eql :ellipse)) params)
  (declare (ignore params))
  (vec-4 0d0 0d0 0d0 1d0))

(defmethod primitive-centroid ((kind (eql :polyline)) params)
  (let ((points (getf params :points))) 
    (if (getf params :closed)
        (centroid-of-polygon points)
        (centroid-of-points points))))

(defmethod primitive-centroid ((kind (eql :path)) params)
  (centroid-of-points (collect-points params)))

(defmethod primitive-centroid ((kind (eql :face)) params)
  (let ((points (getf params :points)))
    (centroid-of-polygon points)))

