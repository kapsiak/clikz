(in-package :quickdraw)


(defun at (name args &optional (viewport *viewport*))
  (delay
   (let* ((e (resolve-name name viewport))
          (f (element-anchor e)))
     (when f (mv-* (element-transform e) (apply f args))))))
