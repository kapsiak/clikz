(in-package :quickdraw)

(defun draw-path (&rest points &key name style &allow-other-keys)
  (emit :path :points 

        (loop for p in points
              for i from 0
              append (list (if (= i 0) :M :L) p))
        :name name
        :style (merge-style *style* style)))


(defun make-parametric-func (func  start end steps)
  (let ((step-size (/ (- end start) steps)))
    (lambda (i)
      (funcall func (* i step-size)))))

(defun make-parametric-path (func  steps)
  (loop for i below steps
        append (list (if (= i 0) :M :L) (funcall func i)))))

(defun draw-path-parametric (func start end steps &key name style)
  (with-style style
    (let ((f (make-parametric-func func start end steps)))
      (emit :path
            :name name
            :points (make-parametric-path f steps)
            :anchor (lambda (&rest args)
                      
                      (let ((head (first args))
                            (rest (rest args)))
                        (ecase head
                          (:midway (funcall f (/ steps 2))))))))))





