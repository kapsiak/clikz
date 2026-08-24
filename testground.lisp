(in-package :quickdraw)

(progn 
  (defun test2 (angle)
    (with-viewport *viewport*
      (with-style '(:stroke-width 4  :stroke blue)
        (with-transform (mat-4-translate 300 300 0)
          (draw-rect 50 50 :rx 0 :ry 0 :name 'r1))
        (with-transform (mat-4-translate 100 100 0)
          (draw-rect 50 50 :rx 0 :ry 0 :name 'r2))
        (draw-segment (at 'r1 '(:center))
                      (at 'r2 '(:center))))))

  (let ((r (make-instance 'svg-backend))
        (e (process #'test2 120)))
    (init-backend r)
    (render r (car e) (cadr e))
    (with-open-file (x "test.svg"
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (write-string (svg-to-string r)  x))))






