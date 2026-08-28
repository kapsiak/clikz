(in-package :quickdraw)

(defparameter *path-tolerance* 1d-4)

(define-primitive path (:world)
  geometry)


(defclass curve ()
  ((segments     :initarg :segments  :reader path-segments)))


(defclass path-segment ()
  ((len :accessor segment-len :initform nil)))


(defclass path-segment-line (path-segment)
  ((start :initarg :start :reader segment-start)
   (end    :initarg :end   :reader segment-end)))

(defclass path-segment-bezier (path-segment)
  ((p0 :initarg :p0 :reader bezier-p0)
   (p1 :initarg :p1 :reader bezier-p1)
   (p2 :initarg :p2 :reader bezier-p2)
   (p3 :initarg :p3 :reader bezier-p3)))

(defmethod deep-walk (func (object curve))
  (make-instance 'curve
    :segments (deep-walk func (path-segments object))))

(defmethod deep-walk (func (object path-segment-line))
  (make-instance 'path-segment-line
    :start (deep-walk func (segment-start object))
    :end (deep-walk func (segment-end object))))

(defmethod deep-walk (func (object path-segment-bezier))
  (make-instance 'path-segment-bezier
    :p0 (deep-walk func (bezier-p0 object))
    :p1 (deep-walk func (bezier-p1 object))
    :p2 (deep-walk func (bezier-p2 object))
    :p3 (deep-walk func (bezier-p3 object))))

(defun path-closed-p (path)
  (let ((segs (path-segments path)))
    (and (> (length segs) 1)
         (<  (magnitude (v- (segment-point (car segs) 0.0d0)
                            (segment-point (car (last segs)) 1.0d0)))
             +epsilon+))))


(defun make-path (segments &key closed)
  (make-instance 'curve :segments
                 (if (not closed)
                     segments
                     (append segments
                             (list (make-instance 'path-segment-line
                                     :start (segment-point (car (last segments)) 1d0)
                                     :end (segment-point (first segments) 0d0)))))))





(defgeneric segment-point (segment u))
(defgeneric segment-derivative (segment u))
(defgeneric segment-length (segment) )
(defgeneric segment-param-at-length (segment l))
(defgeneric segment-reverse (segment))
(defgeneric segment-split (segment u))
(defgeneric segment-morph (a b u))
(defgeneric segment->commands (seg first))


(defun path-length (path)
  (loop for s in (path-segments path)
        sum (segment-length s)))


(defmethod segment-length :around ((s path-segment))
  (declare (ignore))
  (or (segment-len s)
      (setf (segment-len s) (call-next-method))))



;; Segments are easy

(defmethod segment-point ((s path-segment-line) u)
  (lerp (segment-start s) (segment-end s) u))

(defmethod segment-derivative ((s path-segment-line) u)
  (declare (ignore u))
  (v- (segment-end s) (segment-start s)))

(defmethod segment-length ((s path-segment-line))
  (declare (ignore ))
  (magnitude (v- (segment-end s) (segment-start s))))


(defmethod segment-param-at-length ((s path-segment-line) len)
  (declare (ignore ))
  (let ((total (segment-length s)))
    (if (< total +epsilon+) 0d0 (/ len total))))

(defmethod segment-split ((s path-segment-line) u)
  (let ((m (lerp (segment-start s) (segment-end s) u)))
    (values (make-instance 'path-segment-line :start (segment-start s) :end m)
            (make-instance 'path-segment-line :start m :end (segment-end s)))))


(defmethod segment-morph ((seg-1 path-segment-line)
                          (seg-2 path-segment-line) u)
  (make-instance 'path-segment-line
    :start (lerp (segment-start seg-1) (segment-start seg-2) u)
    :end   (lerp (segment-end seg-1)   (segment-end seg-2)   u)))


(defmethod segment->commands ((seg path-segment-line) first)
  (if first
      (list :M (segment-start seg) :L (segment-end seg))
      (list :L (segment-end seg))))

(defmethod segment-reverse ((s path-segment-line))
  (make-instance 'path-segment-line
    :start (segment-end s)
    :end (segment-start s)))


;; Bezier is not


(defun bez3 (p0 p1 p2 p3 u)
  (let ((v (- 1d0 u)))
    (v+ (scale-vec (* v v v) p0)
        (v+ (scale-vec (* 3 v v u) p1)
            (v+ (scale-vec (* 3 v u u) p2)
                (scale-vec (* u u u) p3))))))

(defmethod segment-point ((s path-segment-bezier) u)
  (bez3 (bezier-p0 s)
        (bezier-p1 s)
        (bezier-p2 s)
        (bezier-p3 s) u))

(defmethod segment-derivative ((s path-segment-bezier) u)
  (let ((v (- 1d0 u)))
    (v+ (scale-vec (* 3 v v) (v- (bezier-p1 s) (bezier-p0 s)))
        (v+ (scale-vec (* 6 v u) (v- (bezier-p2 s) (bezier-p1 s)))
            (scale-vec (* 3 u u) (v- (bezier-p3 s) (bezier-p2 s)))))))


(defun bez3-straight-p (p0 p1 p2 p3)
  (< (- (+ (magnitude (v- p1 p0))
           (magnitude (v- p2 p1))
           (magnitude (v- p3 p2)))
        (magnitude (v- p3 p0)))
     *path-tolerance*))


(defun bez3-divide (p0 p1 p2 p3 u)
  (let* ((p01 (lerp p0 p1 u))
         (p12 (lerp p1 p2 u))
         (p23 (lerp p2 p3 u))
         (p012 (lerp p01 p12 u))
         (p123 (lerp p12 p23 u))
         (m (lerp p012 p123 u)))
    (values p0 p01 p012 m p123 p23 p3)))


(defmethod segment-split ((s path-segment-bezier) u)
  (multiple-value-bind (l0 l1 l2 m r2 r1 r3)
      (bez3-divide (bezier-p0 s) (bezier-p1 s) (bezier-p2 s) (bezier-p3 s) u)
    (values (make-instance 'path-segment-bezier :p0 l0 :p1 l1 :p2 l2 :p3 m)
            (make-instance 'path-segment-bezier :p0 m  :p1 r2 :p2 r1 :p3 r3))))

(defun bez3-partial (p u)
  (values (segment-split p u)))


(defmethod segment-length ((s path-segment-bezier))
  (labels ((walk (p0 p1 p2 p3)
             (if (bez3-straight-p p0 p1 p2 p3)
                 (magnitude (v- p3 p0))
                 (multiple-value-bind (l0 l1 l2 m r2 r1 r3)
                     (bez3-divide p0 p1 p2 p3 0.5d0)
                   (+ (walk l0 l1 l2 m) (walk m r2 r1 r3))))))
    (walk (bezier-p0 s) (bezier-p1 s) (bezier-p2 s) (bezier-p3 s))))

(defmethod segment-param-at-length ((s path-segment-bezier) len)
  (let ((total (segment-length s)))
    (if (< total +epsilon+) 0d0
        (labels ((bisect (start end)
                   (let* ((mid (/ (+ start end) 2d0))
                          (sub (bez3-partial s mid))
                          (l (segment-length sub)))
                     (cond ((< (- end start) *path-tolerance*) mid)
                           ((< l len) (bisect mid end))
                           (t (bisect start mid))))))
          (bisect 0d0 1d0)))))

(defmethod segment-reverse ((s path-segment-bezier))
  (make-instance 'path-segment-bezier
    :p0 (bezier-p3 s) :p1 (bezier-p2 s)
    :p2 (bezier-p1 s) :p3 (bezier-p0 s)))


(defmethod segment->commands ((seg path-segment-bezier) first)
  (if first
      (list :M (bezier-p0 seg) :C (bezier-p1 seg) (bezier-p2 seg) (bezier-p3 seg))
      (list :C (bezier-p1 seg) (bezier-p2 seg) (bezier-p3 seg))))


;; basic path constructions 



(defun make-path-line (start end)
  (make-path (list (make-instance 'path-segment-line :start start :end end))))

(defun make-path-bezier (p0 p1 p2 p3)
  (make-path (list (make-instance 'path-segment-bezier
                     :p0 p0 :p1 p1 :p2 p2 :p3 p3))))

(defun path-from-points (points &key closed)
  (make-path
   (loop for tail on points
         for a = (first tail)
         for b = (second tail)
         while b
         collect (make-instance 'path-segment-line :start a :end b))
   :closed closed))



(defun arclength->param (path s)
  (loop for i below (length (path-segments path))
        for seg in (path-segments path)
        with total = 0
        for len = (segment-length seg)
        when (<= s (+ total len))
          return (+ i (segment-param-at-length
                       seg (- s total)))
        do (incf total len)
        finally (return (coerce (length (path-segments path)) 'double-float))))


(defun path-param-at (path u &key (by :fraction))
  (ecase by
    (:parameter u)
    (:arclength (arclength->param path (coerce u 'double-float)))
    (:fraction (arclength->param
                path (* (clamp (coerce u 'double-float) 0d0 1d0)
                        (path-length path))))))


(defun path-segment-at-param (path u)
  (let* ((n (length (path-segments path)))
         (i (clamp (floor u) 0 (1- n))))
    (when (zerop n)
      (error "Path has no segments."))
    (values i (- u i) (nth i (path-segments path)))))



(defun path-point (path u &key (by :fraction))
  (multiple-value-bind (idx local seg)
      (path-segment-at-param path
                             (path-param-at path
                                            u
                                            ;; (* u (length (path-segments path)))
                                            :by by ))
    (declare (ignore idx))
    (segment-point seg local)))



(defun path-tangent (path u &key (by :fraction))
  (multiple-value-bind (idx local seg)
      (path-segment-at-param path (path-param-at path u :by by))
    (declare (ignore idx))
    (segment-derivative seg local)))

(defun frame-normal (tangent &optional up)
  (flet ((get? (test-v)
           (let ((v (v- test-v (scale-vec
                                (dot test-v tangent) tangent))))
             (when (>= (magnitude v) +epsilon+)
               (normalize v)))))
    (or (and up (get? up))
        (get? (vec-4 0 1 0 0))
        (get? (vec-4 0 0 1 0))
        (get? (vec-4 1 0 0 0))
        (error 'zero-vector :vector tangent))))


(defun path-frame (path u &key (by :fraction) (up (vec-4 0 1 0 0)))
  (let* ((point (path-point path u :by by))
         (tangent (normalize (path-tangent path u :by by)))
         (normal (frame-normal tangent up))
         (b (cross-3 (xyz tangent) (xyz normal))))
    (values point tangent normal (vec-4 (vec-x b) (vec-y b) (vec-z b) 0))))

(defun path-points (path)
  (loop for seg in (path-segments path)
        append (list (segment-point seg 0d0)
                     (segment-point seg 0.5d0)
                     (segment-point seg 1d0))))

(defun path->commands (path)
  (let ((commands nil))
    (loop for seg in (path-segments path)
          for first = t then nil
          do (setf commands
                   (append commands (segment->commands seg first))))
    (if (path-closed-p path)
        (append commands (list :Z))
        commands)))


(defun frame->transform (pos tangent normal)
  (let ((b (cross-3 (xyz tangent) (xyz normal))))
    (mat-4-4 (list (vec-x tangent) (vec-x normal) (vec-x b)  (vec-x pos))
             (list (vec-y tangent) (vec-y normal) (vec-y b)  (vec-y pos))
             (list (vec-z tangent) (vec-z normal) (vec-z b)  (vec-z pos))
             (list 0 0 0 1))))


(defun path->svg-commands (path)
  (let* ((segments (path-segments path))
         (is-closed (path-closed-p path))
         (needs-drawing
           (if (and is-closed (typep (car (last segments)) 'path-segment-line))
               (butlast segments)
               segments)))
    (loop for seg in needs-drawing
          for first = t then nil
          append (segment->commands seg first) into ret
          finally
             (return (if is-closed (append ret (list :Z)) ret)))))

(defun path-join (a b)
  (make-path
   (append (copy-list (path-segments a))
           (copy-list (path-segments b)))))

(defun path-reverse (path)
  (make-path
   (mapcar #'segment-reverse (reverse (path-segments path)))))




(defun subpath (path from to &key (by :fraction))
  (let ((seg1-param (path-param-at path from :by by))
        (seg2-param (path-param-at path to :by by)))
    (assert (< seg1-param seg2-param))
    (let* ((segs (path-segments path))
           (n (length segs))
           (idx-1 (clamp (floor seg1-param) 0 (1- n)))
           (idx-2 (clamp (floor seg2-param) 0 (1- n)))
           (param-in-seg1 (- seg1-param idx-1))
           (param-in-seg2 (- seg2-param idx-2)))
      (make-path
       (if (= idx-1 idx-2)
           (let* ((right (nth-value 1 (segment-split (nth idx-1 segs) param-in-seg1)))
                  (span (- 1d0 param-in-seg1))
                  (second (if (< span +epsilon+) 0d0 (/ (- param-in-seg2 param-in-seg1) span))))
             (list  (segment-split right (clamp second 0d0 1d0))))
           (append (list (nth-value 1 (segment-split (nth idx-1 segs) param-in-seg1)))
                   (subseq segs (1+ idx-1) idx-2)
                   (list  (segment-split (nth idx-2 segs) param-in-seg2))))))))

(defun path-shorten (path &key (start 0d0) (end 0d0))
  (let ((total (path-length path)))
    (subpath path
             (clamp (coerce start 'double-float) 0d0 total)
             (clamp (- total (coerce end 'double-float)) 0d0 total)
             :by :arclength)))

(defun path-resample (path n)
  (loop for i from 0 to n
        collect (path-point path (/ (coerce i 'double-float) n))))

(defun warp (path fn steps)
  (path-from-points (mapcar fn (path-resample path steps))))

(defun decorate (path func &key (steps 128) up)
  (let* ((was-closed (path-closed-p path))
         (points (loop for i from 0 to steps
                       for u = (/ (coerce i 'double-float) steps)
                       with cur-vec = nil
                       collect
                       (multiple-value-bind (pt tangent normal)
                           (path-frame path u :up up :by :fraction)
                         (setf cur-vec (funcall func u))
                         (v+ pt
                             (v+ (scale-vec  (vec-x cur-vec) tangent)
                                 (scale-vec  (vec-y cur-vec) normal)))))))
    (path-from-points
     (if was-closed (butlast points) points)
     :closed was-closed)))

(defun decorate-wave (&key (cycles 8) (amplitude 1))
  (lambda (u) (vec-2 0d0 (* amplitude (sin (* 2 pi cycles u))))))


(defun coil-profile (&key (cycles 8) (lead 0.5d0) (amplitude 1.0d0))
  (lambda (u)
    (let ((th (* 2 pi cycles u)))
      (vec-2 (* amplitude (* lead (- (cos th) 1d0)) (sin th))))))



(defun morph (start end u)
  (let ((segs-start (path-segments start))
        (segs-end (path-segments end)))
    (unless (and (= (length segs-start) (length segs-end))
                 (every (lambda (x y) (eq (type-of x) (type-of y)))
                        segs-start segs-end))
      (error "Paths have different structure"))
    (make-path (mapcar (lambda (x y) (segment-morph x y u)) segs-start segs-end))))

(defmethod primitive-anchor ((p path) key &rest args)
  (let ((curve (geometry p)))
    (ecase key
      (:start (path-point curve 0d0))
      (:end   (path-point curve 1d0))
      ((:center :midway) (path-point curve 0.5d0))
      (:at (path-point curve (first args) :by (or (second args) :fraction))))))

(defmethod primitive-centroid ((p path))
  (centroid-of-points (path-points (geometry p))))

(defmethod primitive-sample ((p path) &key (steps 64))
  (path-resample (geometry p) steps))

(defun draw-path (points &key closed name style)
  (emit (make-instance 'path :geometry (path-from-points points :closed closed))
        :name name :style (merge-style *style* style)))

(defun draw-segment (start end &key name style)
  (emit (make-instance 'segment :start start :end end)
        :name name :style (merge-style *style* style)))

(defun draw-polyline (points &key name style)
  (emit (make-instance 'polyline :points points)
        :name name :style (merge-style *style* style)))

(defun make-parametric-func (func start end steps)
  (let ((step-size (/ (- end start) steps)))
    (lambda (i)
      (apply #'p (funcall func (+ start (* i step-size)))))))

(defun make-parametric-path (func  steps)
  (loop for i from 0 to steps
        append (list (if (= i 0) :M :L) (funcall func i))))

(defun draw-path-parametric (func start end steps &key closed name style)
  (let ((f (make-parametric-func func start end steps)))
    (draw-path (loop for i from 0 to steps collect (funcall f i))
               :closed closed :name name :style style)))
