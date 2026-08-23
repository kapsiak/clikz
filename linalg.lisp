(in-package :quickdraw)

(defconstant +epsilon+ 1d-9)

(defvar *linalg-optimize* '((speed 0) (safety 3) (debug 3)))


(defun symbol-for-dims (prefix postfix &rest dims)
  (intern (format nil "~a-~{~d~^-~}~@[-~a~]" (string-upcase prefix)
                  (remove-if #'not dims)
                  (string-upcase postfix))
          :quickdraw))

(defmacro make-matvec (n &optional m? (element-type 'double-float))
  (let ((m (or m? n)))
    `(defun ,(symbol-for-dims "MV" "*" n m?)
         (mat vec
          &optional (result (make-array ,n :element-type ',element-type)))
       (declare (optimize ,@*linalg-optimize*)
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
    `(defun ,(symbol-for-dims "MM" "*" n m? l?)
         (mat1 mat2
          &optional (result (make-array (list ,n ,l) :element-type ',element-type)))
       (declare (optimize ,@*linalg-optimize*)
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
  `(defun ,(symbol-for-dims "V" "*" n) (v1 v2)
     (declare (optimize (speed 3) (safety 0))
              (type (simple-array ,element-type (,n)) v1)
              (type (simple-array ,element-type (,n)) v2))
     (loop for i below ,n
           sum (* (aref v1 i) (aref v2 i)) ,element-type)))


(defmacro make-scale-vec (n &optional (element-type 'double-float))
  `(defun ,(symbol-for-dims "SV" "*" n)
       (val vec
        &optional (result (make-array ,n :element-type ',element-type)))
     (declare (optimize ,@*linalg-optimize*)
              (type ,element-type val)
              (type (simple-array ,element-type (,n)) vec)
              (type (simple-array ,element-type (,n)) result))
     (loop for i below ,n do
       (setf (aref result i) (* val (aref vec i))))
     result))



(defmacro make-vec-norm (n &optional (element-type 'double-float))
  (let ((sv  (symbol-for-dims "SV" "*" n))
        (dot (symbol-for-dims "V" "*" n)))
    `(defun ,(intern (format nil "VNORM-~d-*" n)) (vec)
       (declare (optimize ,@*linalg-optimize*)
                (type (simple-array ,element-type (,n)) vec))
       (,sv (sqrt (,dot vec vec)) vec))))




(defmacro make-vec-+ (n &optional (element-type 'double-float))
  `(defun ,(symbol-for-dims "V" "+" n)
       (vec1 vec2
        &optional (result (make-array ,n :element-type ',element-type)))
     (declare (optimize ,@*linalg-optimize*)
              (type (simple-array ,element-type (,n)) vec1)
              (type (simple-array ,element-type (,n)) vec2)
              (type (simple-array ,element-type (,n)) result))
     (loop for i below ,n do
       (setf (aref result i)
             (+  ( aref vec1 i) ( aref vec2 i))))
     result))

(defmacro make-vec-- (n &optional (element-type 'double-float))
  `(defun ,(symbol-for-dims "V" "-" n)
       (vec1 vec2
        &optional (result (make-array ,n :element-type ',element-type)))
     (declare (optimize ,@*linalg-optimize*)
              (type (simple-array ,element-type (,n)) vec1)
              (type (simple-array ,element-type (,n)) vec2)
              (type (simple-array ,element-type (,n)) result))
     (loop for i below ,n do
       (setf (aref result i)
             (-  ( aref vec1 i) ( aref vec2 i))))
     result))

(defmacro make-scale-mat (n &optional m? (element-type 'double-float))
  (let* ((m (or m? n)))
    `(defun ,(symbol-for-dims "SV" "*" n m)
         (val mat
          &optional (result (make-array (list ,n ,m) :element-type ',element-type)))
       (declare (optimize ,@*linalg-optimize*)
                (type ,element-type val)
                (type (simple-array ,element-type (,n ,m)) mat)
                (type (simple-array ,element-type (,n ,m)) result))
       (loop for i below ,(* n m) do
         (setf (row-major-aref result i) (* val (row-major-aref mat i))))

       result)))

(defmacro make-transpose-mat (n &optional m? (element-type 'double-float))
  (let* ((m (or m? n)))
    `(defun ,(symbol-for-dims "M" "T" n m)
         (mat 
          &optional (result (make-array (list ,m ,n) :element-type ',element-type)))
       (declare (optimize ,@*linalg-optimize*)
                (type (simple-array ,element-type (,n ,m)) mat)
                (type (simple-array ,element-type (,m ,n)) result))
       (loop for i below ,n do
         (loop for j below ,m do
           (setf (aref result j i) (aref mat i j ))))
       result)))


(defmacro make-mat-+ (n &optional m? (element-type 'double-float))
  (let* ((m (or m? n)))
    `(defun ,(symbol-for-dims "M" "+" n m)
         (mat1 mat2
          &optional (result (make-array (list ,n ,m) :element-type ',element-type)))
       (declare (optimize ,@*linalg-optimize*)
                (type (simple-array ,element-type (,n ,m)) mat1)
                (type (simple-array ,element-type (,n ,m)) mat2)
                (type (simple-array ,element-type (,n ,m)) result))
       (loop for i below ,(* n m) do
         (setf (row-major-aref result i)
               (+ (row-major-aref mat1 i) (row-major-aref mat2 i))))

       result)))

(defmacro make-mat-maker (n &optional m? (element-type 'double-float))
  (let ((m (or m? n)))
    `(progn
       (defun ,(symbol-for-dims "MAT" "" n m?) (&rest rows)
         (let ((ret (make-array (list ,n ,m) :element-type ',element-type)))
           (loop for i below ,n for row in rows
                 do (loop for j below ,m for e in row do
                   (setf (aref ret i j) (coerce e ',element-type))))
           ret))
       (defun ,(symbol-for-dims "MAT" "ONES" n m?) ()
         (make-array (list ,n ,m)
                     :initial-element (coerce 1 ',element-type)
                     :element-type ',element-type))
       (defun  ,(symbol-for-dims "MAT" "ZEROS" n m?)()
         (make-array (list ,n ,m)
                     :initial-element (coerce 0 ',element-type)
                     :element-type ',element-type))
       ,(when (= n m)
          `(defun  ,(symbol-for-dims "MAT" "D" n m?) (&rest elements)
             (let ((result   (,(symbol-for-dims "MAT" "ZEROS" n m?))))
               (loop for i below ,n do
                 (setf (aref result i i) 
                       (coerce (if (and elements (= (length elements) ,n))
                                   (nth i elements)
                                   1d0)
                               ',element-type)))
               result))))))


(defmacro make-vec-maker (n &key (element-type 'double-float))
  `(progn
     (defun ,(symbol-for-dims "VEC" nil n)  (&rest elements)
       (let ((ret (make-array (list ,n ) :element-type ',element-type)))
         (loop for i below ,n for e in elements do
           (setf (aref ret i) (coerce e ',element-type)))
         ret))

     (defun ,(symbol-for-dims "VEC" "ONES" n) ()
       (make-array (list ,n) :initial-element 1d0
                             :element-type ',element-type))
     (defun ,(symbol-for-dims "VEC" "ZEROS" n) ()
       (make-array (list ,n) :initial-element 0d0
                             :element-type ',element-type))))


;; (defmacro make-ops-for-dim (n &optional m l (element-type 'double-float))
;;   `(progn
;;      (make-matmat ,n ,m ,l ,element-type)
;;      ,(if (eql l m)
;;           `(progn
;;              (make-matvec ,n ,m ,element-type)
;;      ,(unless l
;;         `(progn 
;;            (make-mat-+ ,n ,m ,element-type)
;;            (make-scale-mat ,n ,m ,element-type)
;;            (make-transpose-mat ,n ,m ,element-type)
;;            ,(unless m
;;               `(progn
;;                  (make-vec-maker ,n :element-type ,element-type)))))))


(defmacro make-linalg  (max-dim)
  (let (forms v+-cases v--cases sv-cases dot-cases norm-cases
        mv-cases mm-cases)
    (loop for i from 2 to max-dim do
      (push `(make-scale-vec ,i) forms)
      (push `(make-vec-+ ,i) forms)
      (push `(make-vec-- ,i) forms)
      (push `(make-vec-norm ,i) forms)
      (push `(make-dot ,i) forms)
      (loop for j from 2 to max-dim do
        (push `(make-mat-maker ,i ,j) forms)
        (push `(make-matvec ,i ,j) forms)
        (push `(make-mat-+ ,i ,j) forms)
        (push `(make-transpose-mat ,i ,j) forms)
        (loop for k from 2 to max-dim do
          (push `(make-matmat ,i ,j ,k) forms)))
      
      (push `(,i (,(symbol-for-dims "V" "+" i) v1 v2)) v+-cases)
      (push `(,i (,(symbol-for-dims "V" "-" i) v1 v2)) v--cases)
      (push `(,i (,(symbol-for-dims "SV" "*"i) val vec)) sv-cases)
      (push `(,i (,(symbol-for-dims "V" "*" i) v1 v2)) dot-cases)
      (push `(,i (,(symbol-for-dims "VNORM" "*" i) vec)) norm-cases)

      (push `(,i (ecase m
                   ,@(loop for j from 2 to max-dim
                           collect `(,j (,(symbol-for-dims "MV" "*" i j)
                                         mat vec)))))
            mv-cases)

      (push `(,i (ecase m
                   ,@(loop for j from 2 to max-dim
                           collect
                           `(,j (ecase l
                                  ,@(loop for k from 2 to max-dim
                                          collect
                                          `(,k (,(symbol-for-dims "MM" "*" i j k) mat1 mat2))
                                          ))))))
            mm-cases))

    `(progn
       ,@(nreverse forms)
       (defun v+ (v1 v2)
         (declare (optimize ,@*linalg-optimize*))
         (ecase (array-dimension v1 0) ,@(nreverse v+-cases)))
       (defun v- (v1 v2)
         (declare (optimize ,@*linalg-optimize*))
         (ecase (array-dimension v1 0) ,@(nreverse v--cases)))
       (defun scale-vec (val vec)
         (declare (optimize ,@*linalg-optimize*))
         (ecase (array-dimension vec 0) ,@(nreverse sv-cases)))
       (defun v* (v1 v2)
         (declare (optimize ,@*linalg-optimize*))
         (ecase (array-dimension v1 0) ,@(nreverse dot-cases)))
       (defun norm (vec)
         (declare (optimize ,@*linalg-optimize*))
         (ecase (array-dimension vec 0) ,@(nreverse norm-cases)))
       (defun mv-* (mat vec)
         (declare (optimize ,@*linalg-optimize*))
         (let ((n (array-dimension mat 0))
               (m (array-dimension mat 1)))
           (ecase n
             ,@(nreverse mv-cases))))
       (defun mm-* (mat1 mat2 &rest rest)
         (declare (optimize ,@*linalg-optimize*))
         (let ((n (array-dimension mat1 0))
               (m (array-dimension mat1 1))
               (l (array-dimension mat2 1)))
           (let ((result 
                   (ecase n
                     ,@(nreverse mm-cases))))
             (if rest
                 (apply #'mm-* result rest)
                 result)))))))


(make-linalg 4)


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
     (list (sin rad) 0  (cos rad) 0)
     (list 0 0 0 1))))

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
          (list 1 0 0 (- (vec-x pos)))
          (list 0 1 0 (- (vec-y pos)))
          (list 0 0 1 (- (vec-z pos)))
          (list 0 0 0 1))))

(defun flatten-2d ()
  (mat-2-4 (list 1 0 0 0)
           (list 0 1 0 0)))

(defun drop-homogenous (v)
  (vec-3 (aref v 0) (aref v 1) (aref v 2)))


(defun cross-2 (a b)
  (- (* (aref a 0) (aref b 1))
     (* (aref a 1) (aref b 0))))

(defun cross-3 (a b)
  (vec-3 (- (* (aref a 1) (aref b 2)) (* (aref a 2) (aref b 1)))
         (- (* (aref a 2) (aref b 0)) (* (aref a 0) (aref b 2)))
         (- (* (aref a 0) (aref b 1)) (* (aref a 1) (aref b 0)))))

(defun cross (a b)
  (ecase (array-dimension a 0)
    (2 (cross-2 a b))
    (3 (cross-3 a b))))

(defun compose-4 (&rest transforms)
  (reduce #'mm-4-* (reverse transforms)))


(defun close-p (val1 val2)
  (< (abs (- val1  val2)) +epsilon+))

(defun affine-p (m)
  (and (close-p (aref m 3 0) 0)
       (close-p (aref m 3 1) 0)
       (close-p (aref m 3 2) 0)))

(defun on-plane-mat (u v p)
  (mat-4-3 (list (vec-x u) (vec-x v) (vec-x p))
           (list (vec-y u) (vec-y v) (vec-y p))
           (list (vec-z u) (vec-z v) (vec-z p))
           (list 0 0 1)))


