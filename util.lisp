(in-package :quickdraw)


(defstruct delayed
  value
  (state :unresolved)
  function) 


(defun resolve (val)
  (cond
    ((not (delayed-p val)) val)
    ((delayed-value val)  (delayed-value val))
    ((eql (delayed-state val) :resolving) (error 'circular))
    (t
     (let ((result (funcall (delayed-function val))))
       (setf (delayed-state val) :resolved)
       (setf (delayed-value val) result)
       result))))

(defmacro delay (&rest body)
  `(make-delayed :value nil :function (lambda () ,@body)))


(defmacro make-delayed-function (name function)
  `(defun ,name (&rest args)
     (if (not (some #'delayed-p args))
         (apply #',function args)
         (delay
          (funcall #',function
                   (mapcar #'resolve args))))))




(defun deep-delayed-p (l)
  (cond
    ((consp l)
     (or (deep-delayed-p (first l)) (deep-delayed-p (rest l))))
    (t
     (delayed-p l))))



(defun deep-resolve (l)
  (cond
    ((consp l)
     (cons (deep-resolve (first l)) (deep-resolve (rest l))))
    (t
     (resolve l))))


