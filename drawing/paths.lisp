(in-package :quickdraw)


(defun draw-path (&rest points &key name style)
  (emit :path :points points
              :name name
              :style (merge-style *style* style)))


(defun make-parametric-path (func  start end steps)
  (let ((step-size (/ (- end start) steps)))
    (loop for i below steps
          append (list (if (= i 0) :M :L) (funcall func (* i step-size))))))

(defun draw-path-parametric (func start end steps &key name style)
  (with-style style
    (emit :path
          :name name
          :points (make-parametric-path func start end steps))))





