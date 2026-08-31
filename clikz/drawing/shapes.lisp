(in-package :clikz)

(define-primitive rect (:local)
  (w :initform 0d0) (h :initform 0d0)
  (rx :initform 0d0) (ry :initform 0d0))

(define-primitive circle (:local)
  (r :initform 0d0))

(define-primitive ellipse (:local)
  (rx :initform 0d0) (ry :initform 0d0))

(define-primitive segment (:world :exact-under :any)
  start end)

(define-primitive polyline (:world :exact-under :any)
  points)

(define-primitive label (:local :exact-under :any)
  (text :initform "")
  (align :initform :center)
  (baseline :initform :middle))

(defmethod primitive-sample ((p label) &key (steps 32))
  (declare (ignore steps))
  (list (vec-4 0 0 0 1)))

(defmethod primitive-centroid ((p segment))
  (centroid-of-points (list (start p) (end p))))


(defmethod primitive-sample ((p segment) &key (steps 32))
  (declare (ignore steps))
  (list (start p) (end p)))

(defmethod primitive-sample ((p polyline) &key (steps 32))
  (declare (ignore steps))
  (points p))

(defmethod primitive-centroid ((p polyline))
  (centroid-of-points (points p)))


(defmethod primitive-anchor ((p rect) key &rest args)
  (declare (ignore args))
  (let ((hw (/ (w p) 2d0)) (hh (/ (h p) 2d0)))
    (ecase key
      (:center (vec-4 0 0 0 1))
      (:north (vec-4 0 hh 0 1)) (:south (vec-4 0 (- hh) 0 1))
      (:east (vec-4 hw 0 0 1)) (:west (vec-4 (- hw) 0 0 1))
      (:ne (vec-4 hw hh 0 1)) (:nw (vec-4 (- hw) hh 0 1))
      (:se (vec-4 hw (- hh) 0 1)) (:sw (vec-4 (- hw) (- hh) 0 1)))))

(defmethod primitive-boundary ((p rect) direction)
  (let* ((hw (/ (w p) 2d0)) (hh (/ (h p) 2d0))
         (dx (vec-x direction)) (dy (vec-y direction))
         (tx (unless (< (abs dx) +epsilon+) (/ hw (abs dx))))
         (ty (unless (< (abs dy) +epsilon+) (/ hh (abs dy))))
         (s (cond ((and tx ty) (min tx ty))
                  (tx tx)
                  (ty ty)
                  (t (error 'zero-vector :vector direction)))))
    (vec-4 (* s dx) (* s dy) 0 1)))

(defmethod primitive-closed-p ((p rect)) t)

(defmethod primitive-sample ((p rect) &key steps)
  (declare (ignore steps))
  (let ((hw (/ (w p) 2d0)) (hh (/ (h p) 2d0)))
    (list (vec-4 (- hw) (- hh) 0 1)
          (vec-4 hw (- hh) 0 1)
          (vec-4 hw hh 0 1)
          (vec-4 (- hw) hh 0 1)
          (vec-4 (- hw) (- hh) 0 1))))

(defun ellipse-bound (rx ry direction)
  (let* ((dx (/ (vec-x direction) rx)) (dy (/ (vec-y direction) ry))
         (m (sqrt (+ (* dx dx) (* dy dy)))))
    (when (< m +epsilon+) (error 'zero-vector :vector direction))
    (vec-4 (/ (vec-x direction) m) (/ (vec-y direction) m) 0 1)))

(defun ellipse-anchor (rx ry key)
  (let ((d (/ (sqrt 2d0) 2)))
    (ecase key
      (:center (vec-4 0 0 0 1))
      (:north (vec-4 0 ry 0 1)) (:south (vec-4 0 (- ry) 0 1))
      (:east (vec-4 rx 0 0 1)) (:west (vec-4 (- rx) 0 0 1))
      (:ne (vec-4 (* rx d) (* ry d) 0 1))
      (:nw (vec-4 (- (* rx d)) (* ry d) 0 1))
      (:se (vec-4 (* rx d) (- (* ry d)) 0 1))
      (:sw (vec-4 (- (* rx d)) (- (* ry d)) 0 1)))))


(defun ellipse-sample (rx ry steps)
  (loop for i to steps
        for th = (* 2 pi (/ i steps))
        collect (vec-4 (* rx (cos th)) (* ry (sin th)) 0 1)))


(defmethod primitive-anchor ((p ellipse) key &rest args)
  (declare (ignore args))
  (ellipse-anchor (rx p) (ry p) key))

(defmethod primitive-boundary ((p ellipse) direction)
  (ellipse-bound (rx p) (ry p) direction))

(defmethod primitive-sample ((p ellipse) &key (steps 32))
  (ellipse-sample (rx p) (ry p)  steps))

(defmethod primitive-anchor ((p circle) key &rest args)
  (declare (ignore args))
  (ellipse-anchor (r p) (r p) key))

(defmethod primitive-boundary ((p circle) direction)
  (ellipse-bound (r p) (r p) direction))

(defmethod primitive-sample ((p circle) &key (steps 32))
  (ellipse-sample (r p) (r p)  steps))

(defun draw-rect (w h &key (rx 0d0) (ry 0d0) name style place)
  (emit (make-instance 'rect :w w :h h :rx rx :ry ry)
        :transform (when place (placement-transform place))
        :name name :style (merge-style *style* style)))

(defun draw-circle (r &key name style place)
  (emit (make-instance 'circle :r r)
        :transform (when place (placement-transform place))
        :name name :style (merge-style *style* style)))

(defun draw-ellipse (rx ry &key name style place)
  (emit (make-instance 'ellipse :rx rx :ry ry)
        :transform (when place (placement-transform place))
        :name name :style (merge-style *style* style)))

(defun draw-label (text &key (align :center) (baseline :middle) name style place)
  (emit (make-instance 'label :text text :align align :baseline baseline)
        :transform (when place (placement-transform place))
        :name name :style (merge-style *style* style)))

(defun draw-regular-polygon (sides r &key name style place)
  (with-transform (if place (placement-transform place) +identity-4+)
    (emit (path-from-points
           (loop for i below sides
                 for theta = (+ (/ (* 2 pi i) sides) (/ pi 2))
                 collect (p (* r (cos theta))
                            (* r (sin theta))))
           :closed t)
          :name name :style (merge-style *style* style))))
