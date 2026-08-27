(in-package :quickdraw)

(defvar *elements* nil)
(defvar *element-index* nil)
(defvar *pending* nil)
(defvar *clip* nil)
(defvar *element-index* nil)
(defvar *resources* (make-hash-table :test #'equal))
(defvar *names* (make-hash-table :test #'equal))
(defvar *viewport* nil)
(defvar *transform* +identity-4+)
(defvar *style* nil)
(defvar *style-list* (make-hash-table))


(defun defstyle (name  style)
  (setf (gethash name *style-list*) (lambda () style))) 

(defun parse-style (style)
  (let (output)
    (loop while (plusp (length style)) 
          for style-key = (pop style) do
            (multiple-value-bind (val found) (gethash style-key *style-list*)
              (if found
                  (setf style (append (funcall val) style))
                  (setf output (append  output (list style-key (pop style)))))))
    output))


(defun merge-style (orig updates)
  (let (( ret (copy-tree  orig)))
    (loop for (key value) on (parse-style updates) by #'cddr
          do
             (setf (getf ret key) value))
    ret))

(defmacro with-transform (transform &rest body)
  `(let ((*transform* (call-delayed #'mm-* *transform* ,transform )))
     ,@body))

(defmacro with-style (style &rest body)
  `(let ((*style* (merge-style *style* ,style)))
     ,@body))

(defmacro with-viewport (v &rest body)
  `(let ((*viewport* ,v))
     ,@body))


(defclass viewport ()
  ((view :initarg :view :reader viewport-view
         :type (array double-float (4 4))) ; 3D homogenous
   (proj :initarg :proj :reader viewport-proj
         :type (array double-float (4 4))) ; 3D homogenous
   (placement :initarg :placement :reader viewport-placement
              :type (array double-float (3 3)))) ; 2D homogenous
  (:default-initargs :view +ortho-z+ :proj +ortho-z+ :placement +placement-identity+))


(defun make-page-viewport (&key (view +ortho-z+) (proj +ortho-z+)
                                (scale 1d0) (origin-x 0d0) (origin-y 0d0)
                                (flip-y t))
  (make-instance 'viewport
    :view view :proj proj
    :placement (mat-3-3 (list scale 0 origin-x)
                        (list 0 (if flip-y (- scale) scale) origin-y)
                        (list 0 0 1))))


(defmethod print-object ((obj viewport) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (obj stream :type t :identity t)
        (format stream ":VIEW ~S ~% :PROJ: ~S :PLACEMENT ~S"
                (if (slot-boundp obj 'view) (viewport-view obj) :unbound)
                (if (slot-boundp obj 'proj) (viewport-proj obj) :unbound)
                (if (slot-boundp obj 'placement) (viewport-placement obj) :unbound)))))



(defun resolve-name (name &optional viewport)
  (resolve (or (gethash (cons name  viewport) *names*)
               (gethash (cons name  *viewport*) *names*)
               (error "Bad key"))))



(defun process (func &rest args)
  (let ((*elements* nil)
        (*pending* nil)
        (*clip* nil)
        (*element-index* 0)
        (*resource-index* 0)
        (*resources* (make-hash-table :test #'equal))
        (*names* (make-hash-table :test #'equal))
        (*viewport* (or *viewport* (make-page-viewport)))
        (*transform* +identity-4+)
        (*style* nil))
    (apply func args)
    (loop while *pending* do
      (resolve (pop *pending*)))
    (list (nreverse *elements*) *resources*)))



(defun placement-transform (place)
  (cond ((delayed-p place) (delay (placement-transform (resolve place))))
        ((and (arrayp place) (= (array-rank place) 2))  place)                  
        (t (mat-4-translate (vec-x place) (vec-y place) (vec-z place)))))

(defun p (&rest coords)
  (if (and (= (length coords) 1) (arrayp (car coords))) 
      (progn (mv-* *transform* (car coords)))
      (mv-* *transform* (apply #'vec-p coords))))

(defun dir (&rest coords)
  (if (and (= (length coords) 1) (arrayp (car coords))) 
      (progn (mv-* *transform* (car coords)))
      (mv-* *transform* (apply #'vec-dir coords))))

(defun emit (primitive &key transform viewport style clip name)
  (let (element
        (s (or style *style*))
        (tr1 *transform*)
        (c (or clip *clip*))
        (idx (incf *element-index*))
        (v (or viewport *viewport*)))
    (flet ((build-element (p)
             (let ((tr (ecase (primitive-space p)
                         (:local (or (resolve transform) (resolve tr1)))
                         (:world +identity-4+))))
               (make-instance 'element
                 :primitive p
                 :transform tr :viewport v :style s :clip c
                 :index idx))))
      (if (or (deep-delayed-p primitive) (delayed-p transform) (delayed-p tr1))
          (progn
            (setq element (delay
                           (first (push (build-element (deep-resolve primitive))
                                        *elements*))))
            (push element *pending*))
          (setq element (first (push (build-element primitive) *elements*))))
      (when name
        (setf (gethash (cons name v) *names*) element))
      element)))

