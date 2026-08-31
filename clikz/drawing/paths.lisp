(in-package :clikz)

(defparameter *path-tolerance* 1d-4)

(define-primitive path (:world)
  segments)


(defclass path-segment ()
  ((len :accessor segment-len :initform nil)))

(defgeneric segment-point (segment u))
(defgeneric segment-derivative (segment u))
(defgeneric segment-length (segment) )
(defgeneric segment-param-at-length (segment l))
(defgeneric segment-reverse (segment))
(defgeneric segment-split (segment u))
(defgeneric segment-morph (a b u))
(defgeneric segment->commands (seg first))


(defclass path-segment-line (path-segment)
  ((start :initarg :start :reader segment-start)
   (end    :initarg :end   :reader segment-end)))

(defclass path-segment-bez3 (path-segment)
  ((p0 :initarg :p0 :reader bez-p0)
   (p1 :initarg :p1 :reader bez-p1)
   (p2 :initarg :p2 :reader bez-p2)
   (p3 :initarg :p3 :reader bez-p3)))

(defclass path-segment-bez2 (path-segment)
  ((p0 :initarg :p0 :reader bez-p0)
   (p1 :initarg :p1 :reader bez-p1)
   (p2 :initarg :p2 :reader bez-p2)))

(defclass path-segment-arc (path-segment)
  ((center :initarg :center :reader arc-center)
   (major  :initarg :major  :reader arc-major)
   (minor  :initarg :minor  :reader arc-minor)
   (start  :initarg :start  :reader arc-start)
   (sweep  :initarg :sweep  :reader arc-sweep)))



(defmethod deep-walk (func (object path-segment-line))
  (make-instance 'path-segment-line
    :start (deep-walk func (segment-start object))
    :end (deep-walk func (segment-end object))))

(defmethod deep-walk (func (object path-segment-bez3))
  (make-instance 'path-segment-bez3
    :p0 (deep-walk func (bez-p0 object))
    :p1 (deep-walk func (bez-p1 object))
    :p2 (deep-walk func (bez-p2 object))
    :p3 (deep-walk func (bez-p3 object))))

(defmethod deep-walk (func (object path-segment-bez2))
  (make-instance 'path-segment-bez2
    :p0 (deep-walk func (bez-p0 object))
    :p1 (deep-walk func (bez-p1 object))
    :p2 (deep-walk func (bez-p2 object))))

(defmethod deep-walk (func (object path-segment-arc))
  (make-instance 'path-segment-arc
    :center (deep-walk func (arc-center object))
    :major      (deep-walk func (arc-major object))
    :minor      (deep-walk func (arc-minor object))
    :start  (deep-walk func (arc-start object))
    :sweep  (deep-walk func (arc-sweep object))))

(defun path-closed-p (path)
  (let ((segs (segments path)))
    (and segs
         (<  (magnitude
              (v- (segment-point (car segs) 0.0d0)
                  (segment-point (car (last segs)) 1.0d0)))
             +epsilon+))))


(defun make-path (segments &key closed)
  (make-instance 'path
    :segments
    (if (not closed)
        segments
        (append segments
                (list (make-instance 'path-segment-line
                        :start (segment-point (car (last segments)) 1d0)
                        :end (segment-point (first segments) 0d0)))))))



(defun path-length (path)
  (loop for s in (segments path)
        sum (segment-length s)))

(defun call-path-points (fn path)
  (deep-walk (lambda (v)
               (if (arrayp v) (funcall fn v) v))
             path))

(defun transform-path-points (mat-4 path)
  (call-path-points (lambda (v) (mv-* mat-4 v))
                    path))


(defmethod segment-length :around ((s path-segment))
  (declare (ignore))
  (or (segment-len s)
      (setf (segment-len s) (call-next-method))))

(defun segment-length-bisect (segment seg-ok-p)
  (labels ((walk (seg)
             (if  (funcall seg-ok-p seg)
                  (magnitude (v- (segment-point seg 0d0) (segment-point seg 1d0)))
                  (multiple-value-bind (r l) (segment-split seg 0.5d0)
                    (+ (walk r) (walk l))))))
    (walk segment)))





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

(defmethod segment-point ((s path-segment-bez3) u)
  (bez3 (bez-p0 s)
        (bez-p1 s)
        (bez-p2 s)
        (bez-p3 s) u))

(defmethod segment-derivative ((s path-segment-bez3) u)
  (let ((v (- 1d0 u)))
    (v+ (scale-vec (* 3 v v) (v- (bez-p1 s) (bez-p0 s)))
        (v+ (scale-vec (* 6 v u) (v- (bez-p2 s) (bez-p1 s)))
            (scale-vec (* 3 u u) (v- (bez-p3 s) (bez-p2 s)))))))


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


(defmethod segment-split ((s path-segment-bez3) u)
  (multiple-value-bind (l0 l1 l2 m r2 r1 r3)
      (bez3-divide (bez-p0 s) (bez-p1 s) (bez-p2 s) (bez-p3 s) u)
    (values (make-instance 'path-segment-bez3 :p0 l0 :p1 l1 :p2 l2 :p3 m)
            (make-instance 'path-segment-bez3 :p0 m  :p1 r2 :p2 r1 :p3 r3))))

(defun bez3-partial (p u)
  (values (segment-split p u)))


;; (defmethod segment-length ((s path-segment-bez3))
;;   (labels ((walk (p0 p1 p2 p3)
;;              (if (bez3-straight-p p0 p1 p2 p3)
;;                  (magnitude (v- p3 p0))
;;                  (multiple-value-bind (l0 l1 l2 m r2 r1 r3)
;;                      (bez3-divide p0 p1 p2 p3 0.5d0)
;;                    (+ (walk l0 l1 l2 m) (walk m r2 r1 r3))))))
;;     (walk (bez-p0 s) (bez-p1 s) (bez-p2 s) (bez-p3 s))))
(defmethod segment-length ((s path-segment-bez3))
  (segment-length-bisect s #'(lambda (x)
                               (bez3-straight-p (bez-p0 x) (bez-p1 x)
                                                (bez-p2 x) (bez-p3 x)))))
;; (labels ((walk (p0 p1 p2 p3)
;;            (if (bez3-straight-p p0 p1 p2 p3)
;;                (magnitude (v- p3 p0))
;;                (multiple-value-bind (l0 l1 l2 m r2 r1 r3)
;;                    (bez3-divide p0 p1 p2 p3 0.5d0)
;;                  (+ (walk l0 l1 l2 m) (walk m r2 r1 r3))))))
;;   (walk (bez-p0 s) (bez-p1 s) (bez-p2 s) (bez-p3 s))))

(defmethod segment-param-at-length ((s path-segment-bez3) len)
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


(defmethod segment-morph ((a path-segment-bez3) (b path-segment-bez3) u)
  (make-instance 'path-segment-bez3
    :p0 (lerp (bez-p0 a) (bez-p0 b) u)
    :p1 (lerp (bez-p1 a) (bez-p1 b) u)
    :p2 (lerp (bez-p2 a) (bez-p2 b) u)
    :p3 (lerp (bez-p3 a) (bez-p3 b) u)))

(defmethod segment-reverse ((s path-segment-bez3))
  (make-instance 'path-segment-bez3
    :p0 (bez-p3 s) :p1 (bez-p2 s)
    :p2 (bez-p1 s) :p3 (bez-p0 s)))


(defmethod segment->commands ((seg path-segment-bez3) first)
  (if first
      (list :M (bez-p0 seg) :C (bez-p1 seg) (bez-p2 seg) (bez-p3 seg))
      (list :C (bez-p1 seg) (bez-p2 seg) (bez-p3 seg))))


(defun bez2 (p0 p1 p2 u)
  (let ((v (- 1d0 u)))
    (v+ (scale-vec (* v v) p0)
        (v+ (scale-vec (* 2 v u) p1)
            (scale-vec (* u u) p2)))))

(defmethod segment-point ((s path-segment-bez2) u)
  (bez2 (bez-p0 s) (bez-p1 s) (bez-p2 s) u))

(defmethod segment-derivative ((s path-segment-bez2) u)
  (let ((v (- 1d0 u)))
    (v+ (scale-vec (* 2 v) (v- (bez-p1 s) (bez-p0 s)))
        (scale-vec (* 2 u) (v- (bez-p2 s) (bez-p1 s))))))

(defun bez2-straight-p (p0 p1 p2)
  (< (- (+ (magnitude (v- p1 p0))
           (magnitude (v- p2 p1)))
        (magnitude (v- p2 p0)))
     *path-tolerance*))

(defun bez2-divide (p0 p1 p2 u)
  (let* ((p01 (lerp p0 p1 u))
         (p12 (lerp p1 p2 u))
         (m (lerp p01 p12 u)))
    (values p0 p01 m p12 p2)))

(defmethod segment-split ((s path-segment-bez2) u)
  (multiple-value-bind (l0 l1 m r1 r2)
      (bez2-divide (bez-p0 s) (bez-p1 s) (bez-p2 s) u)
    (values (make-instance 'path-segment-bez2 :p0 l0 :p1 l1 :p2 m)
            (make-instance 'path-segment-bez2 :p0 m  :p1 r1 :p2 r2))))

;; (defmethod segment-length ((s path-segment-bez2))
;;   (labels ((walk (p0 p1 p2)
;;              (if (bez2-straight-p p0 p1 p2)
;;                  (magnitude (v- p2 p0))
;;                  (multiple-value-bind (l0 l1 m r1 r2)
;;                      (bez2-divide p0 p1 p2 0.5d0)
;;                    (+ (walk l0 l1 m) (walk m r1 r2))))))
;;     (walk (bez-p0 s) (bez-p1 s) (bez-p2 s))))


(defmethod segment-length ((s path-segment-bez2))
  (segment-length-bisect s #'(lambda (x)
                               (bez2-straight-p (bez-p0 x) (bez-p1 x)
                                                (bez-p2 x)))))

(defmethod segment-param-at-length ((s path-segment-bez2) len)
  (let ((total (segment-length s)))
    (if (< total +epsilon+) 0d0
        (labels ((bisect (start end)
                   (let* ((mid (/ (+ start end) 2d0))
                          (l (segment-length (values (segment-split s mid)))))
                     (cond ((< (- end start) *path-tolerance*) mid)
                           ((< l len) (bisect mid end))
                           (t (bisect start mid))))))
          (bisect 0d0 1d0)))))

(defmethod segment-reverse ((s path-segment-bez2))
  (make-instance 'path-segment-bez2
    :p0 (bez-p2 s) :p1 (bez-p1 s) :p2 (bez-p0 s)))

(defmethod segment-morph ((a path-segment-bez2) (b path-segment-bez2) u)
  (make-instance 'path-segment-bez2
    :p0 (lerp (bez-p0 a) (bez-p0 b) u)
    :p1 (lerp (bez-p1 a) (bez-p1 b) u)
    :p2 (lerp (bez-p2 a) (bez-p2 b) u)))

(defmethod segment->commands ((seg path-segment-bez2) first)
  (if first
      (list :M (bez-p0 seg) :Q (bez-p1 seg) (bez-p2 seg))
      (list :Q (bez-p1 seg) (bez-p2 seg))))



(defun arc-angle-at (s u)
  (+ (arc-start s) (* u (arc-sweep s))))

(defun arc-point-at-angle (s theta)
  (v+ (arc-center s)
      (v+
       (scale-vec (cos theta) (arc-major s))
       (scale-vec (sin theta) (arc-minor s)))))

(defun arc-derivative-at-angle (s theta)
  (v+ (scale-vec (- (sin theta)) (arc-major s))
      (scale-vec (cos theta) (arc-minor s))))

(defmethod segment-point ((s path-segment-arc) u)
  (arc-point-at-angle s (arc-angle-at s u)))

(defmethod segment-derivative ((s path-segment-arc) u)
  (scale-vec (arc-sweep s)
             (arc-derivative-at-angle s (arc-angle-at s u))))

(defun arc-circular-p (s)
  (let* ((l1 (magnitude (arc-major s)))
         (l2 (magnitude (arc-minor s)))
         (scale (max l2 l1)))
    (and (> scale +epsilon+)
         (< (abs (- l1 l2)) (* +epsilon+ scale))
         (< (abs (dot (arc-major s) (arc-minor s))) (* +epsilon+ scale scale)))))


(defmethod segment-length ((s path-segment-arc))
  (if (arc-circular-p s)
      (* (magnitude (arc-major s)) (abs (arc-sweep s)))
      (segment-length-bisect s
                             (lambda (x) 
                               (let* ((m (segment-point x 0.5d0))
                                      (start (segment-point x 0d0))
                                      (end (segment-point x 1d0))
                                      (l (+ (magnitude (v- start m))
                                            (magnitude (v- end m)))))
                                 (< (abs (- l (magnitude (v- start end))))
                                    *path-tolerance*))))))

(defmethod segment-param-at-length ((s path-segment-arc) len)
  (let ((total (segment-length s)))
    (cond
      ((< total +epsilon+) 0d0)
      ((arc-circular-p s) (/ len total))
      (t (labels ((bisect (start end)
                    (let* ((mid (/ (+ start end) 2d0))
                           (l (segment-length (segment-split s mid))))
                      (cond ((< (- end start) *path-tolerance*) mid)
                            ((< l len) (bisect mid end))
                            (t (bisect start mid))))))
           (bisect 0d0 1d0))))))

(defmethod segment-split ((s path-segment-arc) u)
  (let ((mid (arc-angle-at s u)))
    (values (make-instance 'path-segment-arc
              :center (arc-center s) :major (arc-major s) :minor (arc-minor s)
              :start (arc-start s) :sweep (* u (arc-sweep s)))
            (make-instance 'path-segment-arc
              :center (arc-center s) :major (arc-major s) :minor (arc-minor s)
              :start mid :sweep (* (- 1d0 u) (arc-sweep s))))))

(defmethod segment-reverse ((s path-segment-arc))
  (make-instance 'path-segment-arc
    :center (arc-center s)
    :major (arc-major s)
    :minor (arc-minor s)
    :start (arc-angle-at s 1d0)
    :sweep (- (arc-sweep s))))

(defmethod segment-morph ((a path-segment-arc) (b path-segment-arc) u)
  (make-instance 'path-segment-arc
    :center (lerp (arc-center a) (arc-center b) u)
    :major      (lerp (arc-major a) (arc-major b) u)
    :minor      (lerp (arc-minor a) (arc-minor b) u)
    :start  (+ (* (- 1d0 u) (arc-start a)) (* u (arc-start b)))
    :sweep  (+ (* (- 1d0 u) (arc-sweep a)) (* u (arc-sweep b)))))

(defun arc->bez3 (s)
  (let* ((n (max 1 (ceiling (abs (arc-sweep s)) (/ pi 2))))
         (step (/ (arc-sweep s) n))
         (alpha (* (/ 4d0 3d0) (tan (/ step 4d0)))))
    (loop for i below n
          for a0 = (+ (arc-start s) (* i step))
          for a1 = (+ a0 step)
          for q0 = (arc-point-at-angle s a0)
          for q1 = (arc-point-at-angle s a1)
          collect (make-instance 'path-segment-bez3
                    :p0 q0
                    :p1 (v+ q0 (scale-vec alpha (arc-derivative-at-angle s a0)))
                    :p2 (v- q1 (scale-vec alpha (arc-derivative-at-angle s a1)))
                    :p3 q1))))

(defmethod segment->commands ((seg path-segment-arc) first)
  (loop for c in (arc->bez3 seg)
        for head = first then nil
        append (segment->commands c head)))


;; basic path constructions



(defun make-path-line (start end)
  (make-path (list (make-instance 'path-segment-line :start start :end end))))

(defun make-path-bez3 (p0 p1 p2 p3)
  (make-path (list (make-instance 'path-segment-bez3
                     :p0 p0 :p1 p1 :p2 p2 :p3 p3))))

(defun make-path-bez2 (p0 p1 p2)
  (make-path (list (make-instance 'path-segment-bez2
                     :p0 p0 :p1 p1 :p2 p2))))

(defun make-arc-segment (center major minor &key (start 0d0) (sweep (* 2 pi)))
  (make-instance 'path-segment-arc
    :center center :major major :minor minor
    :start (coerce start 'double-float)
    :sweep (coerce sweep 'double-float)))

(defun make-path-arc (center rx ry start sweep &key (rotation 0d0))
  (let* ((theta (deg->rad rotation))
         (c (cos theta))
         (s (sin theta)))
    (make-path
     (list (make-arc-segment center
                             (dir (* rx c) (* rx s))
                             (dir (* (- ry) s) (* ry c))
                             :start (deg->rad start)
                             :sweep (deg->rad sweep))))))

(defun make-path-circle (center r)
  (make-path-arc center r r 0 360d0))

(defun path-from-points (points &key closed)
  (make-path
   (loop for tail on points
         for a = (first tail)
         for b = (second tail)
         while b
         collect (make-instance 'path-segment-line :start a :end b))
   :closed closed))



(defun arclength->param (path s)
  (loop for i below (length (segments path))
        for seg in (segments path)
        with total = 0
        for len = (segment-length seg)
        when (<= s (+ total len))
          return (+ i (segment-param-at-length
                       seg (- s total)))
        do (incf total len)
        finally (return (coerce (length (segments path)) 'double-float))))


(defun path-param-at (path u &key (by :fraction))
  (ecase by
    (:parameter u)
    (:arclength (arclength->param path (coerce u 'double-float)))
    (:fraction (arclength->param
                path (* (clamp (coerce u 'double-float) 0d0 1d0)
                        (path-length path))))))


(defun path-segment-at-param (path u)
  (let* ((n (length (segments path)))
         (i (clamp (floor u) 0 (1- n))))
    (when (zerop n)
      (error "Path has no segments."))
    (values i (- u i) (nth i (segments path)))))



(defun path-point (path u &key (by :fraction))
  (multiple-value-bind (idx local seg)
      (path-segment-at-param path
                             (path-param-at path
                                            u
                                            ;; (* u (length (segments path)))
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
  (loop for seg in (segments path)
        append (list (segment-point seg 0d0)
                     (segment-point seg 0.5d0)
                     (segment-point seg 1d0))))

(defun path->commands (path)
  (let ((commands nil))
    (loop for seg in (segments path)
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
  (let* ((segments (segments path))
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
   (append (copy-list (segments a))
           (copy-list (segments b)))))

(defun path-reverse (path)
  (make-path
   (mapcar #'segment-reverse (reverse (segments path)))))




(defun subpath (path from to &key (by :fraction))
  (let ((seg1-param (path-param-at path from :by by))
        (seg2-param (path-param-at path to :by by)))
    (assert (< seg1-param seg2-param))
    (let* ((segs (segments path))
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
  (let ((segs-start (segments start))
        (segs-end (segments end)))
    (unless (and (= (length segs-start) (length segs-end))
                 (every (lambda (x y) (eq (type-of x) (type-of y)))
                        segs-start segs-end))
      (error "Paths have different structure"))
    (make-path (mapcar (lambda (x y) (segment-morph x y u)) segs-start segs-end))))

(defmethod primitive-anchor ((p path) key &rest args)
  (ecase key
    (:start (path-point p 0d0))
    (:end   (path-point p 1d0))
    ((:center :midway) (path-point p 0.5d0))
    (:at (path-point p (first args) :by (or (second args) :fraction)))))

(defmethod primitive-centroid ((p path))
  (centroid-of-points (path-points p)))

(defmethod primitive-sample ((p path) &key (steps 64))
  (path-resample p steps))

(defun draw-path (points &key closed name style)
  (emit (path-from-points points :closed closed)
        :name name :style (merge-style *style* style)))

(defun draw-curve (path &key name style)
  "Emit an already-built PATH -- the entry point for the curve combinators."
  (emit path :name name :style (merge-style *style* style)))

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
