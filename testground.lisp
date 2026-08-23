(in-package :quickdraw)

(progn 
  (defun test2 (angle)
    (with-viewport *viewport*
      (with-style '(:stroke "blue" :stroke-width 1 :fill "none")
        (with-transform (mat-4-rot-x angle)
          (with-transform (mat-4-translate 200 200 30)
            (draw-surface (lambda (x y) (p (* 150 x) (* 150 y)
                                           (* -200 (expt 2.71 (- (+ (expt x 2) (expt (* 2 y) 2)))))))
                          :u0 -2 :u1 2 :v0 -2 :v1 2 :mode :face
                          :style '(:fill red)
                          :back-style '(:fill orange))
            )))))

  (let ((r (make-instance 'svg-backend))
        (e (process #'test2 20)))
    (init-backend r)
    (render r (car e) (cadr e))
    (with-open-file (x "test.svg"
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (write-string (svg-to-string r)  x))))






