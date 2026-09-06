(in-package :clikz)

;; Markers are a general case of a page picture.
;; insets coulb maybe be used later for trimming.

(defun marker-inset (shape size)
  (* size (or (get shape 'marker-inset) 0d0)))


(defun arrowhead (&key (size 10d0) (width 0.75d0))
  (let ((hw (* 0.5d0 size width)))
    (draw-path (list (p 0 0) (p (- size) hw) (p (- size) (- hw)))
               :closed t)))
(setf (get 'arrowhead 'marker-inset) 1d0)

;; Tikz stealth head
(defun stealth-head (&key (size 12d0) (width 0.75d0) (notch 0.35d0))
  (let ((hw (* 0.5d0 size width)))
    (draw-path (list (p 0 0)
                     (p (- size) hw)
                     (p (* (- size) (- 1d0 notch)) 0)
                     (p (- size) (- hw)))
               :closed t)))
(setf (get 'stealth-head 'marker-inset) 0.65d0)

(defun open-head (&key (size 10d0) (width 0.75d0))
  (let ((hw (* 0.5d0 size width)))
    (draw-path (list (p (- size) hw) (p 0 0) (p (- size) (- hw))))))
(setf (get 'open-head 'marker-inset) 0d0)

(defun dot-marker (&key (size 8d0))
  (draw-circle (* 0.5d0 size)))
(setf (get 'dot-marker 'marker-inset) 0d0)


(defun draw-marker (shape &key at along (size 10d0) (scale 1d0) pivot style)
  "Think warpper for with-page-picture"
  (with-page-picture (:at at :along along :scale scale
                      :pivot (or pivot (vec-2 0 0)))
    (with-style style
      (funcall shape :size size))))


(defun draw-path-marker (name u &key (shape 'arrowhead) (size 10d0)
                                     (by :fraction) style)

  "Common case of adding arrow to path, does not trim path or anything, so may look a bit funny."
  (path-point-at name u :by by)
  (draw-marker shape
               :at (path-point-at name u :by by)
               :along (path-tangent-at name u :by by)
               :size size :style style))

