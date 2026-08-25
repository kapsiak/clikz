(in-package :quickdraw)


(defstruct delayed
  value
  (state :unresolved)
  function
  source)





(define-condition circular-dependency (error)
  ((delayed :initarg :delayed :reader circular-dependency-delayed)
   (source :initarg :source :initform nil :reader circular-dependency-source))
  (:report (lambda (c s)
             (format s "Circular dependency while resolving ~s"
                     (or (circular-dependency-source c)
                         (circular-dependency-delayed c))))))

(defun resolve (val)
  (if (not (delayed-p val))
      val
      (ecase (delayed-state val)
        (:resolved (delayed-value val))
        (:resolving (error 'circular-dependency
                           :delayed val :source (delayed-source val)))
        (:unresolved
         (setf (delayed-state val) :resolving)
         (let ((result (funcall (delayed-function val))))
           (setf (delayed-state val) :resolved)
           (setf (delayed-value val) result)
           result)))))

(defmacro delay (&rest body)
  `(make-delayed :value nil :function (lambda () ,@body)
                 :source ',body))


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
    ((consp l) (cons (deep-resolve (first l)) (deep-resolve (rest l))))
    (t (resolve l))))

(defun deep-call (func l &key (merge #'cons))
  (cond
    ((consp l) (funcall merge (funcall func (first l)) (deep-call func (rest l))))
    (t (funcall func l))))

