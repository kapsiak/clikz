(in-package :quickdraw)

(defmacro scope (args &rest body)
  (let ((x (getf args :x 0d0))
        (y (getf args :y 0d0))
        (z (getf args :z 0d0))
        (rot-x (getf args :rot-x 0d0))
        (rot-y (getf args :rot-y 0d0))
        (rot-z (getf args :rot-z 0d0)))
    `(with-transform (mm-*
                      (mat-4-rot-z ,rot-z)
                      (mat-4-rot-y ,rot-y)
                      (mat-4-rot-x ,rot-x)
                      (mat-4-translate ,x ,y ,z))
       (with-style style
         ,@body))))

(defun connect (a b &key arrow-start arrow-end)
  (let ((p (draw-path (list (toward a b) (toward b a)))))
    (when arrow-start (draw-path-marker  (resolve p) 0.0 :shape arrow-start))
    (when arrow-end (draw-path-marker  (resolve p) 1.0 :shape arrow-end))))
