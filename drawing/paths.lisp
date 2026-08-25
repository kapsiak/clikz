(in-package :quickdraw)


(defclass path ()
  ((segments     :initarg :segments  :reader path-segments)
   (closed       :initarg :closed    :initform nil :reader path-closed-p)
   (length-table :accessor path-length-table :initform nil)
   (total-length :accessor path-total-length :initform nil)))

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
(defgeneric segment-length (segment &key tolerance))

(defmethod segment-length :around ((s path-segment) &key tolerance)
  (declare (ignore tolerance))
  (or (segment-len s)
      (setf (segment-len s) (call-next-method))))

(defmethod segment-point ((s path-segment-line) u)
  (lerp (segment-start s) (segment-end s) u))

(defmethod segment-derivative ((s path-segment-line) u)
  (declare (ignore u))
  (v- (segment-end s) (segment-start s)))

(defmethod segment-length ((s path-segment-line) &key tolerance)
  (declare (ignore tolerance))
  (magnitude (v- (segment-end s) (segment-start s))))

(defun bez3 (p0 p1 p2 p3 u)
  (let ((v (- 1d0 u)))
    (v+ (scale-vec (* v v v) p0)
        (v+ (scale-vec (* 3 v v u) p1)
            (v+ (scale-vec (* 3 v u u) p2)
                (scale-vec (* u u u) p3))))))

(defmethod segment-point ((s path-segment-bezier) u)
  (bez3 (bezier-p0 s) (bezier-p1 s) (bezier-p2 s) (bezier-p3 s) u))

(defmethod segment-derivative ((s path-segment-bezier) u)
  (let* ((v (- 1d0 u)))
    (v+ (scale-vec (* 3 v v) (v- (bezier-p1 s) (bezier-p0 s)))
        (v+ (scale-vec (* 6 v u) (v- (bezier-p2 s) (bezier-p1 s)))
            (scale-vec (* 3 u u) (v- (bezier-p3 s) (bezier-p2 s)))))))

(defun bez3-straight-p (p0 p1 p2 p3 tolerance)
  (< (- (+ (magnitude (v- p1 p0))
           (magnitude (v- p2 p1))
           (magnitude (v- p3 p2)))
        (magnitude (v- p3 p0)))
     tolerance))

(defun bez3-divide (p0 p1 p2 p3 u)
  (let* ((p01 (lerp p0 p1 u))
         (p12 (lerp p1 p2 u))
         (p23 (lerp p2 p3 u))
         (p012 (lerp p01 p12 u))
         (p123 (lerp p12 p23 u))
         (m (lerp p012 p123 u)))
    (values p0 p01 p012 m p123 p23 p3)))


(defmethod segment-length ((s path-segment-bezier) &key (tolerance 1d-4))
  (labels ((walk (p0 p1 p2 p3)
             (if (bez3-straight-p p0 p1 p2 p3 tolerance)
                 (magnitude (v- p3 p0))
                 (multiple-value-bind (l0 l1 l2 m r2 r1 r3)
                     (bez3-divide p0 p1 p2 p3 0.5d0)
                   (+ (walk l0 l1 l2 m) (walk m r2 r1 r3))))))
    (walk (bezier-p0 s) (bezier-p1 s) (bezier-p2 s) (bezier-p3 s))))


(defun path-point (path u &key (by :arclength) (tolerance 1d-4))
  (multiple-value-bind (idx local seg)
      (path-segment-at-param path (param-at path u :by by :tolerance tolerance))
    (declare (ignore idx))
    (segment-point seg local)))


(defun path-length (path &key (tolerance 1d-4))
  (or (path-total-length path)
      (setf (path-total-length path)
            (loop for seg in (path-segments path)
                  sum (segment-length seg
                                      :tolerance tolerance)))))

(defun param-at (path u &key (by :arclength) (tolerance 1d-4))
  (ecase by
    (:parameter  u)
    (:arclength  (arclength->param path u :tolerance tolerance))))

(defun path-segment-at-param (path u)
  (let* ((n (length (path-segments path)))
         (idx (min (1- n) (max 0 (floor u))))
         (local (- u idx)))
    (values idx local (nth idx (path-segments path)))))

;; (defun arclength->param (path s &key (tolerance 1d-4))
;;   (loop for i from 0 for seg in (path-segments path)
;;         for len = (segment-length seg :tolerance tolerance)
;;         when (<= s len)
;;           return (+ i (segment-local-param-at-length seg s :tolerance tolerance))
;;         do (decf s len)
;;         finally (return (length (path-segments path)))))


(defun path-tangent (path u &key (by :arclength))
  (multiple-value-bind (idx local seg)
      (path-segment-at-param path (param-at path u :by by))
    (declare (ignore idx))
    (segment-derivative seg local)))


(defun path-frame (path u &key (by :arclength)
                               (up (vec-4 0 1 0 0)))
  (let* ((point (path-point path u :by by))
         (tangent (normalize (path-tangent path u :by by)))
         (normal (normalize
                  (v- up
                      (scale-vec (dot up tangent) tangent))))
         (b (cross-3 (xyz tangent) (xyz normal))))
    (values point tangent normal (vec-4 (vec-x b) (vec-y b) (vec-z b) 0))))




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

