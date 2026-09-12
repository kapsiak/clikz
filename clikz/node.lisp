(in-package :clikz)

(defparameter *node-shapes*
  '(:rectangle :rounded :circle :ellipse :none))

(define-primitive coordinate (:local :exact-under :any))

(defmethod primitive-centroid ((p coordinate)) (vec-4 0 0 0 1))
(defmethod primitive-anchor ((p coordinate) key &rest args)
  (declare (ignore key args))
  (vec-4 0 0 0 1))
(defmethod primitive-boundary ((p coordinate) direction)
  (declare (ignore direction))
  (vec-4 0 0 0 1))
(defmethod primitive-sample ((p coordinate) &key steps)
  (declare (ignore steps))
  (list (vec-4 0 0 0 1)))
(defmethod primitive-cull-p ((p coordinate) element)
  (declare (ignore element))
  t)



(defun node-shape-primitive (shape w h rounded)
  (ecase shape
    (:rectangle (make-instance 'rect :w w :h h))
    (:rounded   (make-instance 'rect :w w :h h :rx rounded :ry rounded))
    (:circle    (make-instance 'circle :r (/ (sqrt (+ (* w w) (* h h))) 2d0)))
    (:ellipse   (make-instance 'ellipse :rx (* w (/ (sqrt 2d0) 2d0))
                               :ry (* h (/ (sqrt 2d0) 2d0))))
    (:diamond   (make-instance 'diamond :w (* 2d0 w) :h (* 2d0 h)))
    (:none      (make-instance 'coordinate))))

(defun node-direction (d)
  (ecase  d
    (:above       (values :north :south  0d0  1d0))
    (:below       (values :south :north  0d0 -1d0))
    (:left        (values :west  :east  -1d0  0d0))
    (:right       (values :east  :west   1d0  0d0))
    (:above-left  (values :nw :se -1d0  1d0))
    (:above-right (values :ne :sw  1d0  1d0))
    (:below-left  (values :sw :ne -1d0 -1d0))
    (:below-right (values :se :nw  1d0 -1d0))))

(defun node-placement (direction ref dist)
  (multiple-value-bind (ref-anchor self-anchor dx dy) (node-directions direction)
    (values (delay (v+ (resolve (at ref ref-anchor))
                       (scale-vec (to-df dist)
                                  (normalize (vec-dir dx dy 0d0)))))
            self-anchor)))

(defun draw-node (text &key
                         (shape :rectangle)
                         name
                         at
                         anchor
                         rel ; (:above name dist)
                         draw
                         fill 
                         style
                         size
                         inner-sep
                         inner-xsep
                         inner-ysep
                         (min-width 0)
                         (min-height 0)
                         (min-size 0)
                         (rounded 3d0)
                         layer)
  (let* ((text (text->svg->text text))
         (em (to-df (or size (text-size style))))
         (inner-sep (to-df (or inner-sep (* 0.4d0 em))))
         (inner-hsep (to-df (or inner-xsep inner-sep)))
         (inner-vsep (to-df (or inner-ysep inner-sep)))
         (dir (when rel (node-direction (first rel))))
         (gap (to-df (or (third rel) (getf style :sep) 20d0))))
    (multiple-value-bind (text-w text-h text-d)
        (if text (measure-text text :size em) (values 0d0 0d0 0d0))
      (let ((w (max (to-df min-width) (to-df min-size) (+ text-w (* 2d0 inner-hsep))))
            (h (max (to-df min-height) (to-df min-size) (+ text-h text-d (* 2d0 inner-vsep)))))
        (multiple-value-bind (point own-anchor)
            (if rel
                (node-placement dir (second rel) gap)
                (values at :center))
          (let* ((prim (node-shape-primitive shape w h (to-df rounded)))
                 (shift (primitive-anchor prim (or anchor own-anchor))))
            (with-at point
              (with-transform (mat-4-translate (- (vec-x shift))
                                               (- (vec-y shift)) 0d0)
                (let ((element (emit prim :name name :style style
                                          :layer layer)))
                  (when text
                    (draw-text-prim text :size em 
                                         :align :center :baseline :middle
                                         :style style))
                  
                  element)))))))))

(defun draw-coordinate (position &key name)
  (with-at position
    (emit (make-instance 'coordinate) :name name)))
