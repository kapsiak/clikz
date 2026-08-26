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



(defgeneric deep-walk (func object))

(defmethod deep-walk (func (object t))
  (funcall func object))

(defmethod deep-walk (func (object cons))
  (cons (funcall func (first object))
        (deep-walk func (rest object))))


(defun deep-delayed-p (l)
  (deep-walk
   (lambda (x) (when (delayed-p x)
                 (return-from deep-delayed-p t)))
   l)
  nil)

(defun deep-resolve (l)
  (deep-walk #'resolve l))

(defun call-delayed (func &rest args)
  (if (deep-delayed-p args)
      (delay (apply func (deep-resolve args)))
      (apply func args)))


(defun clamp (val low high)
  (max low (min val high)))

