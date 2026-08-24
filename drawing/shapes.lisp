(in-package :quickdraw)


(defun rect-anchors (w h)
  (let ((hw (/ w 2d0)) (hh (/ h 2d0)))
    (lambda (key &rest args)
      (declare (ignore args))
      (ecase key
        (:center (vec-4 0 0 0 1))
        (:north (vec-4 0 hh 0 1)) (:south (vec-4 0 (- hh) 0 1))
        (:east (vec-4 hw 0 0 1)) (:west (vec-4 (- hw) 0 0 1))
        (:ne (vec-4 hw hh 0 1)) (:nw (vec-4 (- hw) hh 0 1))
        (:se (vec-4 hw (- hh) 0 1)) (:sw (vec-4 (- hw) (- hh) 0 1))))))

(defun ellipse-anchors (rx ry)
  (lambda (key &rest args)
    (declare (ignore args))
    (let ((d (/ (sqrt 2d0) 2)))
      (ecase key
        (:center (vec-4 0 0 0 1))
        (:north (vec-4 0 ry 0 1)) (:south (vec-4 0 (- ry) 0 1))
        (:east (vec-4 rx 0 0 1)) (:west (vec-4 (- rx) 0 0 1))
        (:ne (vec-4 (* rx d) (* ry d) 0 1))
        (:nw (vec-4 (- (* rx d)) (* ry d) 0 1))
        (:se (vec-4 (* rx d) (- (* ry d)) 0 1))
        (:sw (vec-4 (- (* rx d)) (- (* ry d)) 0 1))))))

(defun ellipse-boundary (rx ry)
  (lambda (dir)
    (let* ((dx (/ (vec-x dir) rx)) (dy (/ (vec-y dir) ry))
           (m (sqrt (+ (* dx dx) (* dy dy)))))
      (when (< m +epsilon+) (error 'zero-vector :vector dir))
      (vec-4 (/ (vec-x dir) m) (/ (vec-y dir) m) 0 1))))


(defun draw-rect (w h &key rx ry name style)
  (emit :rect
        (list :w w :h h :rx rx :ry ry)
        :name name :style (merge-style *style* style)
        :anchor (rect-anchors w h)
        :boundary nil));(rect-boundary w h)))

(defun draw-circle (r &key name style)
  (emit :circle
        (list :r r)
        :name name :style (merge-style *style* style)
        :anchor (ellipse-anchors r r)
        :boundary (ellipse-boundary r r)))

(defun draw-ellipse (rx ry &key name style)
  (emit :ellipse
        (list :rx rx :ry ry)
        :name name :style (merge-style *style* style)
        :anchor (ellipse-anchors rx ry)
        :boundary (ellipse-boundary rx ry)))

(defun draw-label (text &key (align :center) (baseline :middle) name style)
  (emit :label (list :text text :align align :baseline baseline)
        :name name :style (merge-style *style* style)))
