(in-package :quickdraw)

(defmacro with-current-viewport (v &rest body)
  `(let ((,v *viewport*))
     ,@body))

(defun at (name &rest args)
  (with-current-viewport v
    (delay
     (let* ((e (resolve-name name v))
            (f (element-anchor e)))
       (when f 
         (mv-* (element-transform e) (apply f args)))))))

(defun toward (a b)
  (with-current-viewport v
    (delay
     (let* ((elem-a (resolve-name a v))
            (elem-b (resolve-name b v))
            (center-a (mv-* (element-transform elem-a)
                            (funcall (element-anchor elem-a) :center)))
            (center-b (mv-* (element-transform elem-b)
                            (funcall (element-anchor elem-b) :center)))
            (world-dir (v- center-b center-a))
            (local-dir (mv-* (invert-4 (element-transform elem-a)) world-dir)))
       (unless (element-boundary elem-a)
         (error "Element ~s has no boundary function." a))
       (mv-* (element-transform elem-a)
             (funcall (element-boundary elem-a) local-dir))))))

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
     (let* ((e (resolve-name name v))
            (f (element-anchor e)))
       (when f 
         (v+ by
             (mv-* (element-transform e) (apply f args))))))))
