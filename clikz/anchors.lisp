(in-package :clikz)

(defmacro with-current-viewport (v &rest body)
  "Capture current viewport. Use for possible delayed objects."
  `(let ((,v *viewport*))
     ,@body))


(defun get-anchor-in-world (name viewport args)
  (let ((e (resolve-element name viewport)))
    (mv-* (elem->world e)
          (apply #'element-anchor-point e (or args '(:center))))))

(defun get-boundary-in-world (name viewport d)
  (let ((e (resolve-element name viewport)))
    (mv-* (elem->world e)
          (apply #'element-boundary-point e d))))

(defun at (name &rest args)
  (with-current-viewport v
    (delay
     (get-anchor-in-world name v args))))


(defmacro gen-rel-pos-fun (fname dir default-anchor)
  `(defun ,fname (name &rest args)
     (let ((sep (getf *style* :sep))
           (args (or args (list ,default-anchor))))
       (with-current-viewport v
         (delay
          (v+ (scale-vec (coerce sep 'double-float) ,dir)
              (get-anchor-in-world name v args)))))))


(gen-rel-pos-fun above (vec-dir 0 1) :north)
(gen-rel-pos-fun below (vec-dir 0 -1) :south)
(gen-rel-pos-fun right (vec-dir 1 0) :east)
(gen-rel-pos-fun left (vec-dir -1 0) :west)


(defun toward (a b)
  "Determine point on boundary closest to other object, then invert to model frame"
  (with-current-viewport v
    (delay
     (let* ((elem-a (resolve-element a v))
            (elem-b (resolve-element b v))
            (center-a (mv-* (elem->world elem-a)
                            (element-anchor-point elem-a :center)))
            (center-b (mv-* (elem->world elem-b)
                            (element-anchor-point elem-b :center)))
            (world-dir (v- center-b center-a))
            (local-dir (mv-* (invert-4 (elem->world elem-a)) world-dir)))
       (mv-* (elem->world elem-a)
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
           (mv-* (elem->world e)
                 (apply #'element-anchor-point e args)))))))


(defun path-of (name &optional viewport)
  (element-primitive (resolve-element name viewport)))

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




