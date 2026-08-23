(in-package :quickdraw)

(defun path-command-list (points &key closed)
  (let ((cmds (loop for p in points
                    for i from 0
                    append (list (if (= i 0) :M :L) p))))
    (if closed (append cmds (list :Z)) cmds)))

(defun draw-path (&rest points &key name style &allow-other-keys)
  (emit :path :points 
        (path-command-list points)
        :name name
        :style (merge-style *style* style)))


(defun draw-segment (p0 p1 &key name style)
  (emit :segment :p0 p0 :p1 p1
        :name name :style (merge-style *style* style)))

(defun draw-polyline (points &key name style closed)
  (emit :polyline :points points :closed closed
        :name name :style (merge-style *style* style)))


(defun make-parametric-func (func  start end steps)
  (let ((step-size (/ (- end start) steps)))
    (lambda (i)
      (funcall func (* i step-size)))))

(defun make-parametric-path (func  steps)
  (loop for i below steps
        append (list (if (= i 0) :M :L) (funcall func i))))

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

