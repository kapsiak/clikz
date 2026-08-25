(in-package :quickdraw)


(defun at (name &rest args)
  (delay
   (let* ((e (resolve-name name *viewport*))
          (f (element-anchor e)))
     (when f 
        (mv-* (element-transform e) (apply f args))))))

(defun toward (a b &optional (viewport *viewport*))
  (delay
   (let* ((elem-a (resolve-name a viewport))
          (elem-b (resolve-name b viewport))
          (center-a (mv-* (element-transform elem-a)
                          (funcall (element-anchor elem-a) :center)))
          (center-b (mv-* (element-transform elem-b)
                          (funcall (element-anchor elem-b) :center)))
          ;; (x (print elem-a))
          ;; (x (print (element-transform elem-a)))
          ;; (x (print center-a))
          ;; (x (print center-b))
          (world-dir (v- center-b center-a))
          (local-dir (mv-* (invert-4 (element-transform elem-a)) world-dir)))
     (unless (element-boundary elem-a)
       (error "Element ~s has no boundary function." a))
     (mv-* (element-transform elem-a)
           (funcall (element-boundary elem-a) local-dir)))))
