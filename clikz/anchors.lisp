(in-package :quickdraw)

(defmacro with-current-viewport (v &rest body)
  `(let ((,v *viewport*))
     ,@body))

(defun at (name &rest args)
  (with-current-viewport v
    (delay
     (let ((e (resolve-element name v)))
       (mv-* (element-transform e)
             (apply #'element-anchor-point e args))))))

(defun toward (a b)
  (with-current-viewport v
    (delay
     (let* ((elem-a (resolve-element a v))
            (elem-b (resolve-element b v))
            (center-a (mv-* (element-transform elem-a)
                            (element-anchor-point elem-a :center)))
            (center-b (mv-* (element-transform elem-b)
                            (element-anchor-point elem-b :center)))
            (world-dir (v- center-b center-a))
            (local-dir (mv-* (invert-4 (element-transform elem-a)) world-dir)))
       (mv-* (element-transform elem-a)
             (element-boundary-point elem-a local-dir))))))

(defun between (place1 place2 u)
  (delay
   (let* ((p1 (resolve place1))
          (p2 (resolve place2)))
     (lerp p1 p2 u))))

(defun midpoint (place1 place2)
  (between place1 place2 0.5d0))

(defun shifted-by (name by &rest args)
  (with-current-viewport v
    (delay
     (let ((e (resolve-element name v)))
       (v+ by
           (mv-* (element-transform e)
                 (apply #'element-anchor-point e args)))))))


(defun path-of (name &optional viewport)
  (geometry (element-primitive (resolve-element name viewport))))

(defun path-length-at (name)
  (with-current-viewport v
    (delay (path-length (path-of name v)))))

(defun path-point-at (name u &key (by :fraction))
  (with-current-viewport v
    (delay (path-point (path-of name v) (resolve u) :by by))))

(defun path-tangent-at (name u &key (by :fraction))
  (with-current-viewport v
    (delay (path-tangent (path-of name v) (resolve u) :by by))))

(defun path-frame-at (name u &key (by :fraction) up)
  (with-current-viewport v
    (delay
     (multiple-value-bind (pt tangent normal)
         (path-frame (path-of name v) (resolve u) :by by :up up)
       (frame->transform pt tangent normal)))))



(defun connect (a b &key arrow-start arrow-end)
  (let ((p (draw-path (list (toward a b) (toward b a)))))
    (when arrow-start (draw-path-marker  (resolve p) 0.0 :shape arrow-start))
    (when arrow-end (draw-path-marker  (resolve p) 1.0 :shape arrow-end))))

