(in-package :quickdraw)

(define-primitive face (:world :exact-under :any)
  points normal back-style (cull :initform t))


(defmethod primitive-sample ((p face) &key (steps 32))
  (declare (ignore steps))
  (points p))


(defmethod primitive-centroid ((p face))
  (centroid-of-polygon (points p)))

(defmethod primitive-face-side ((p face) element)
  (let ((n (normal p)))
    (when n
      (let* ((eye (elem->eye element))
             (nz (vec-z (mv-* eye (vec-4 (vec-x n) (vec-y n) (vec-z n) 0)))))
        (if (> nz 0) :front :back)))))

(defmethod primitive-cull-p ((p face) element)
  (and (cull p)
       (eql (primitive-face-side p element) :back)))


(defun sample-surface (f u0 u1 steps-u v0 v1 steps-v)
  (let ((u-step (/ (- u1 u0) steps-u))
        (v-step (/ (- v1 v0) steps-v)))
    (flet ((idx->u (i) (+ u0 (* i u-step)))
           (idx->v (i) (+ v0 (* i v-step))))
      (loop for i from 0 to steps-v
            collect (loop for j from 0 to steps-u
                          collect (apply #'p (funcall f
                                              (idx->u j)
                                              (idx->v i))))))))

(defun quad-normal (p00 p10 p01)
  (normalize (cross-3 (v- p10 p00) (v- p01 p00))))

(defun draw-surface-wire (grid &key style)
  (loop for row in grid
        do (emit (make-instance 'polyline :points row)
                 :style (merge-style *style* style)))
  (loop for j from 0 below (length (first grid))
        do (emit (make-instance 'polyline
                   :points (loop for row in grid collect (nth j row)))
                 :style (merge-style *style* style))))

(defun draw-surface-faces (grid &key style back-style (cull (not back-style)))
  (loop for i from 0 below (1- (length grid))
        for row  = (nth i grid)
        for next = (nth (1+ i) grid)
        do (loop for j from 0 below (1- (length row))
                 for p00 = (nth j row)
                 for p10 = (nth (1+ j) row)
                 for p11 = (nth (1+ j) next)
                 for p01 = (nth j next)
                 do (emit (make-instance 'face
                            :points (list p00 p10 p11 p01)
                            :normal (quad-normal p00 p10 p01)
                            :back-style (and back-style
                                             (merge-style *style* back-style))
                            :cull cull)
                          :style (merge-style *style* style)))))


(defun draw-surface (f &key (u0 0d0) (u1 1d0) (steps-u 20)
                            (v0 0d0) (v1 1d0) (steps-v 20)
                            (mode :wire)  style back-style (cull t))
  (let ((grid (sample-surface f u0 u1 steps-u v0 v1 steps-v)))
    (ecase mode
      (:wire (draw-surface-wire grid :style style))
      (:face (draw-surface-faces grid :style style
                                      :back-style back-style
                                      :cull cull)))))
