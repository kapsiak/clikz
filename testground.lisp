(in-package :quickdraw)

(progn 
  (defun test2 ()
    (with-viewport *viewport*
      (with-style '(:stroke "red" :stroke-width 4 :fill "none")
        (print *transform*)
        (with-transform (mat-4-rot-x -45)
          (with-transform (mat-4-translate 100 0 20)
            (draw-path-parametric (lambda (x) (p (* 50 (sin x)) (* 50 (cos x)) (* 20 x))) 0 50 200)
            )))))

  (let ((r (make-instance 'svg-backend))
        (e (process #'test2)))
    (init-backend r)
    (loop for elem in (first e) do
      (render-element r (element-type elem) elem))
    (with-open-file (x "test.svg"
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (write-string (svg-to-string r)  x)))

  )






(m-4-* (mat-4-rot-x -90) (mat-4-translate (p 100 100)))
(m-4-*  (mat-4-translate (p 100 100)) (mat-4-rot-x -90))
