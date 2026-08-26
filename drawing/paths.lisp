(in-package :quickdraw)

(defparameter *path-tolerance* 1d-4)

(defclass path ()
  ((segments     :initarg :segments  :reader path-segments)))

(defun make-path (segments &key closed)
  (make-instance 'path :segments 
                 (if (not closed)
                     segments
                     (append segments
                             (list (make-instance 'path-segment-line
                                     :start (segment-point (car (last segments)) 1)
                                     :end (segment-point (first segments) 0)))))))

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





(defgeneric segment-point (segment u))
(defgeneric segment-derivative (segment u))
(defgeneric segment-length (segment) )
(defgeneric segment-param-at-length (segment l))
(defgeneric segment-reverse (seg))
(defgeneric segment->commands (seg first))


;;; Path operations

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

(defun bez3-partial (p u)
  (multiple-value-bind (l0 l1 l2 m r2 r1 r3)
      (bez3-divide (bezier-p0 p) (bezier-p1 p) (bezier-p2 p) (bezier-p3 p) u)
    (declare (ignore r1 r2 r3))
    (make-instance 'path-segment-bezier :p0 l0 :p1 l1 :p2 l2 :p3 m)))


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


(defun path-param-at (path u &key (by :arclength))
  (ecase by
    (:parameter  u)
    (:arclength  (arclength->param path u))))


(defun path-segment-at-param (path u)
  (let* ((n (length (path-segments path)))
         (i (clamp (floor u) 0 (1- n))))
    (when (zerop n)
      (error "Path has no segments."))
    (values i (- u i) (nth i (path-segments path)))))



(defun path-point (path u &key (by :arclength))
  (multiple-value-bind (idx local seg)
      (path-segment-at-param path
                             (path-param-at path
                                            (* u (length (path-segments path)))
                                            :by by ))
    (declare (ignore idx))
    (segment-point seg local)))



(defun path-tangent (path u &key (by :arclength))
  (multiple-value-bind (idx local seg)
      (path-segment-at-param path (path-param-at path u :by by))
    (declare (ignore idx))
    (segment-derivative seg local)))


(defun path-frame (path u &key (by :arclength) (up (vec-4 0 1 0 0)))
  
  (let* ((point (path-point path u :by by))
         (tangent (normalize (path-tangent path u :by by)))
         (normal (normalize
                  (v- up
                      (scale-vec (dot up tangent) tangent))))
         (b (cross-3 (xyz tangent) (xyz normal))))
    (values point tangent normal (vec-4 (vec-x b) (vec-y b) (vec-z b) 0))))

(defun path->commands (path)
  (let ((commands nil))
    (loop for seg in (path-segments path)
          for first = t then nil
          do (setf commands
                   (append commands (segment->commands seg first))))
    (if (path-closed-p path)
        (append commands (list :Z))
        commands)))

(defun path-join (a b)
  (make-path
   (append (copy-list (path-segments a))
           (copy-list (path-segments b)))))

(defun path-reverse (path)
  (make-path
   (mapcar #'segment-reverse (reverse (path-segments path)))))

(defun frame->transform (pos tangent normal)
  (let ((b (cross-3 (xyz tangent) (xyz normal))))
    (mat-4-4 (list (vec-x tangent) (vec-x normal) (vec-x b)  (vec-x pos))
             (list (vec-y tangent) (vec-y normal) (vec-y b)  (vec-y pos))
             (list (vec-z tangent) (vec-z normal) (vec-z b)  (vec-z pos))
             (list 0 0 0 1))))



(defun points->command-list (points &key closed)
  (let ((cmds (loop for p in points
                    for i from 0
                    append (list (if (= i 0) :M :L) p))))
    (if closed (append cmds (list :Z)) cmds)))


(defun draw-path (points &key closed name style)
  (emit :path
        (list :points (points->command-list points :closed closed))
        :name name :style (merge-style *style* style)))

(defun draw-segment (start end &key name style)
  (emit :segment
        (list :start start :end end)
        :name name :style (merge-style *style* style)))

(defun draw-polyline (points &key closed name style)
  (emit :polyline
        (list :points points :closed closed)
        :name name
        :style (merge-style *style* style)))


(defun make-parametric-func (func start end steps)
  (let ((step-size (/ (- end start) steps)))
    (lambda (i)
      (apply #'p (funcall func (+ start (* i step-size)))))))

(defun make-parametric-path (func  steps)
  (loop for i from 0 to steps
        append (list (if (= i 0) :M :L) (funcall func i))))

(defun draw-path-parametric (func start end steps &key name style)
  (let ((f (make-parametric-func func start end steps)))
    (emit :path
          (list :points (make-parametric-path f steps))
          :name name
          :style (merge-style *style* style)
          :anchor (lambda (&rest args)
                    (let ((head (first args))
                          (rest (rest args)))
                      (declare (ignore rest))
                      (ecase head
                        (:start (funcall f 0))
                        (:end (funcall f steps))
                        (:midway (funcall f (/ steps 2)))))))))

