(in-package :quickdraw)


(defmacro speedmode ()
  ``(optimize (speed 0) (safety 3)))

(defmacro make-matvec (n &optional m? (element-type 'double-float))
  (let ((m (or m? n)))
    `(defun ,(intern (format nil "MV-~d~@[-~d~]-*" n m?))
         (mat vec
          &optional (result (make-array ,n :element-type ',element-type)))
       (declare ,(speedmode)
                (type (simple-array ,element-type (,n ,m)) mat)
                (type (simple-array ,element-type (,n)) result)
                (type (simple-array ,element-type (,m)) vec))
       (loop for i below ,n do
         (setf (aref result i) 
               (loop for j below ,m
                     sum (* (aref vec j) (aref mat i j)) ,element-type)))
       result)))



(defmacro make-matmat (n &optional m? l? (element-type 'double-float))
  (let* ((m (or m? n))
         (l (or l? m)))
    `(defun ,(intern (format nil "M-~d~@[-~d~]~@[-~d~]-*" n m? l?))
         (mat1 mat2
          &optional (result (make-array (list ,n ,l) :element-type ',element-type)))
       (declare ,(speedmode)
                (type (simple-array ,element-type (,n ,m)) mat1)
                (type (simple-array ,element-type (,m ,l)) mat2)
                (type (simple-array ,element-type (,n ,l)) result))

       (loop for i below ,n do
         (loop for k below ,l do
           (setf (aref result i k)
                 (loop for j below ,m 
                       sum (* (aref mat1 i j) (aref mat2 j k)) ,element-type))))
       result)))

(defmacro make-dot (n &optional (element-type 'double-float))
  `(defun ,(intern (format nil "V-~d-*" n)) (v1 v2)
     (declare (optimize (speed 3) (safety 0))
              (type (simple-array ,element-type (,n)) v1)
              (type (simple-array ,element-type (,n)) v2))
     (let (result)
       (loop for i below ,n
             sum (* (aref v1 i) (aref v2 i)) ,element-type)
       result)))



(defmacro make-scale-vec (n &optional (element-type 'double-float))
  `(defun ,(intern (format nil "SV-~d-*" n))
       (val vec
        &optional (result (make-array ,n :element-type ',element-type)))
     (declare ,(speedmode)
              (type ,element-type val)
              (type (simple-array ,element-type (,n)) vec)
              (type (simple-array ,element-type (,n)) result))
     (loop for i below ,n do
       (setf (aref result i) (* val (aref vec i))))
     result))

(defmacro make-vec-+ (n &optional (element-type 'double-float))
  `(defun ,(intern (format nil "V-~d-+" n))
       (vec1 vec2
        &optional (result (make-array ,n :element-type ',element-type)))
     (declare ,(speedmode)
              (type (simple-array ,element-type (,n)) vec1)
              (type (simple-array ,element-type (,n)) vec2)
              (type (simple-array ,element-type (,n)) result))
     (loop for i below ,n do
       (setf (aref result i)
             (+  ( aref vec1 i) ( aref vec2 i))))
     result))

(defmacro make-scale-mat (n &optional m? (element-type 'double-float))
  (let* ((m (or m? n)))
    `(defun ,(intern (format nil "SM-~d~@[-~d~]-*" n m?))
         (val mat
          &optional (result (make-array (list ,n ,m) :element-type ',element-type)))
       (declare ,(speedmode)
                (type ,element-type val)
                (type (simple-array ,element-type (,n ,m)) mat)
                (type (simple-array ,element-type (,n ,m)) result))
       (loop for i below ,(* n m) do
         (setf (row-major-aref result i) (* val (row-major-aref mat i))))

       result)))

(defmacro make-transpose-mat (n &optional m? (element-type 'double-float))
  (let* ((m (or m? n)))
    `(defun ,(intern (format nil "M-~d~@[-~d~]-T" n m?))
         (mat 
          &optional (result (make-array (list ,m ,n) :element-type ',element-type)))
       (declare ,(speedmode)
                (type (simple-array ,element-type (,n ,m)) mat)
                (type (simple-array ,element-type (,m ,n)) result))
       (loop for i below ,n do
         (loop for j below ,m do
           (setf (aref result j i) (aref mat i j ))))
       result)))


(defmacro make-mat-+ (n &optional m? (element-type 'double-float))
  (let* ((m (or m? n)))
    `(defun ,(intern (format nil "M-~d~@[-~d~]-+" n m?))
         (mat1 mat2
          &optional (result (make-array (list ,n ,m) :element-type ',element-type)))
       (declare ,(speedmode)
                (type (simple-array ,element-type (,n ,m)) mat1)
                (type (simple-array ,element-type (,n ,m)) mat2)
                (type (simple-array ,element-type (,n ,m)) result))
       (loop for i below ,(* n m) do
         (setf (row-major-aref result i)
               (+ (row-major-aref mat1 i) (row-major-aref mat2 i))))

       result)))

(defmacro make-mat-maker (dim &key dim2 (element-type 'double-float)  postfix )
  (let ((f (format nil "MAT-~d~@[-~d~]~@[-~a~]" dim dim2 postfix))
        (d2 (or dim2 dim)))
    `(progn
       (defun ,(intern f) (&rest rows)
         (let ((ret (make-array (list ,dim ,d2) :element-type ',element-type)))
           (loop for i below ,dim for row in rows
                 do (loop for j below ,d2 for e in row do
                   (setf (aref ret i j) (coerce e ',element-type))))
           ret))
       (defun ,(intern (concatenate 'string f "-ONES")) ()
         (make-array (list ,dim ,d2)
                     :initial-element (coerce 1 ',element-type)
                     :element-type ',element-type))
       (defun ,(intern (concatenate 'string f "-ZEROS")) ()
         (make-array (list ,dim ,d2)
                     :initial-element (coerce 0 ',element-type)
                     :element-type ',element-type))
       ,(when (= dim d2)
          `(defun ,(intern (concatenate 'string f "-D")) (&rest elements)
             (let ((result   (,(intern (concatenate 'string f "-ZEROS")))))
               (print result)
               (loop for i below ,dim do
                 (print elements)
                 (setf (aref result i i) 
                       (coerce (if (and elements (= (length elements) ,dim))
                                   (nth i elements)
                                   1d0)
                               ',element-type)))
               result))))))


(defmacro make-vec-maker (dim &key (element-type 'double-float) postfix)
  (let ((f (format nil "VEC-~d~@[~a~]" dim postfix)))
    `(progn
       (defun ,(intern f) (&rest elements)
         (let ((ret (make-array (list ,dim ) :element-type ',element-type)))
           (loop for i below ,dim for e in elements do
             (setf (aref ret i) (coerce e ',element-type)))
           ret))

       (defun ,(intern (concatenate 'string f "-ONES")) ()
         (make-array (list ,dim) :initial-element 1d0
                                 :element-type ',element-type))
       (defun ,(intern (concatenate 'string f "-ZEROS")) ()
         (make-array (list ,dim) :initial-element 0d0
                                 :element-type ',element-type)))))


(defmacro make-ops-for-dim (n &optional m (element-type 'double-float))
  `(progn
     (make-mat-+ ,n ,m ,element-type)
     (make-scale-mat ,n ,m ,element-type)
     (make-matmat ,n ,m nil ,element-type)
     (make-mat-maker ,n :dim2 ,m :element-type ,element-type)
     (make-transpose-mat ,n ,m ,element-type)
     (make-matvec ,n ,m ,element-type)
     ,(unless m
        `(progn
           (make-scale-vec ,n ,element-type)
           (make-vec-+ ,n ,element-type)
           (make-dot ,n ,element-type)
           (make-vec-maker ,n :element-type ,element-type)))))


(make-ops-for-dim 2)
(make-ops-for-dim 3)
(make-ops-for-dim 4)

(make-ops-for-dim 3 2)
(make-ops-for-dim 2 3)

(make-ops-for-dim 3 4)
(make-ops-for-dim 4 3)

(make-ops-for-dim 4 2)
(make-ops-for-dim 2 4)
(make-matmat 3 3 4 double-float)

(defun mm-* (mat1 mat2 &rest rest)
  (declare 
   (type (simple-array double-float (* *)) mat1)
   (type (simple-array double-float (* *)) mat2))
  (let* ((n (array-dimension mat1 0))
         (m (array-dimension mat2 1))
         (l (array-dimension mat2 1))
         (result (make-array (list n l) :element-type 'double-float)))
    (loop for i below n do
      (loop for k below l do
        (setf (aref result i k)
              (loop for j below m
                    sum (* (aref mat1 i j) (aref mat2 j k)) double-float))))
    (if (not rest)
        result
        (apply #'mm-* result rest))))

(defun mv-* (mat vec)
  (declare 
   (type (simple-array double-float (* *)) mat)
   (type (simple-array double-float (*)) vec))
  (let* ((n (array-dimension mat 0))
         (m (array-dimension mat 1))
         (result (make-array (list n) :element-type 'double-float)))
    (loop for i below n do
      (setf (aref result i) 
            (loop for j below m
                  sum (* (aref vec j) (aref mat i j)) double-float)))
    result))



(defun vec-x (vec) (aref vec 0))
(defun vec-y (vec) (aref vec 1))
(defun vec-z (vec) (aref vec 2))
(defun vec-w (vec) (aref vec 3))



(defun deg-to-rad (deg)
  (* (/ deg 360)  2 pi))

(defun mat-2-rot (angle)
  (let ((rad (deg-to-rad angle)))
    (mat-2 (list (cos rad) (- (sin rad)))
           (list (sin rad) (cos rad)))))

(defun mat-3-rot-x (angle)
  (let ((rad (deg-to-rad angle)))
    (mat-3
     (list 1 0 0)
     (list 0 (cos rad) (- (sin rad)))
     (list  0 (sin rad) (cos rad)))))

(defun mat-3-rot-y (angle)
  (let ((rad (deg-to-rad angle)))
    (mat-3
     (list (cos rad) 0 (- (sin rad)))
     (list 0 1 0)
     (list  (sin rad) 0 (cos rad)))))

(defun mat-3-rot-z (angle)
  (let ((rad (deg-to-rad angle)))
    (mat-3
     (list (cos rad) (- (sin rad)) 0 )
     (list (sin rad) (cos rad) 0)
     (list 0 0 1))))

(defun mat-4-rot-x (angle)
  (let ((rad (deg-to-rad angle)))
    (mat-4
     (list 1 0 0 0)
     (list 0 (cos rad) (- (sin rad)) 0)
     (list  0 (sin rad) (cos rad) 0)
     (list 0 0 0 1))))

(defun mat-4-rot-y (angle)
  (let ((rad (deg-to-rad angle)))
    (mat-4
     (list (cos rad) 0  (- (sin rad)) 0)
     (list 0 1 0 0)
     (list (sin rad) 0  (cos rad) 0))
    (list 0 0 0 1)))

(defun mat-4-rot-z (angle)
  (let ((rad (deg-to-rad angle)))
    (mat-4
     (list (cos rad)  (- (sin rad)) 0 0)
     (list (sin rad)   (cos rad)  0 0 )
     (list 0 0 1 0)
     (list 0 0 0 1))))

(defun mat-4-translate (x y z)
  (mat-4
   (list 1 0 0 x)
   (list 0 1 0 y)
   (list 0 0 1 z)
   (list 0 0 0 1)))


(defun look-at (pos at up right)
  (m-4-* (mat-4
          (list (vec-x up) (vec-y up) (vec-z up) 0)
          (list (vec-x right) (vec-y right) (vec-z right) 0)
          (list (vec-x at) (vec-y at) (vec-z at) 0)
          (list 0 0 0 1))

         (mat-4
          (list 0 0 0 (- (vec-x pos)))
          (list 0 0 0 (- (vec-y pos)))
          (list 0 0 0 (- (vec-z pos)))
          (list 0 0 0 1))))

(defun flatten-2d ()
  (mat-2-4 (list 1 0 0 0)
           (list 0 1 0 0)))


(defun compose-4 (&rest transforms)
  (reduce #'m-4-* (reverse transforms)))
