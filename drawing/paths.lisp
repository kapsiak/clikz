(in-package :quickdraw)

(defun points->command-list (points &key closed)
  (let ((cmds (loop for p in points
                    for i from 0
                    append (list (if (= i 0) :M :L) p))))
    (if closed (append cmds (list :Z)) cmds)))

(defun draw-path (points &key closed name style)
  (emit :path
        (list :points (points->command-list points :closed closed))
        :name name :style (merge-style *style* style)))

(defun draw-segment (start end &key name style)
  (emit :segment
        (list :start start :end end)
        :name name :style (merge-style *style* style)))

(defun draw-polyline (points &key closed name style)
  (emit :polyline
        (list :points points :closed closed)
        :name name
        :style (merge-style *style* style)))


(defun make-parametric-func (func start end steps)
  (let ((step-size (/ (- end start) steps)))
    (lambda (i)
      (funcall func (+ start (* i step-size))))))

(defun make-parametric-path (func  steps)
  (loop for i from 0 to steps
        append (list (if (= i 0) :M :L) (funcall func i))))

(defun draw-path-parametric (func start end steps &key name style)
    (let ((f (make-parametric-func func start end steps)))
      (emit :path
            (list :points (make-parametric-path f steps))
            :name name
            :style (merge-style *style* style)
            :anchor (lambda (&rest args)
                      (let ((head (first args))
                            (rest (rest args)))
                        (ecase head
                          (:start (funcall f 0))
                          (:end (funcall f steps))
                          (:midway (funcall f (/ steps 2)))))))))

