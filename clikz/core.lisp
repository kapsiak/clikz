(in-package :clikz)

(defparameter *page-frame-step-size* 1d-3)

(defvar *elements* nil)
(defvar *element-index* nil)
(defvar *pending* nil)
(defvar *clip* nil)
(defvar *element-index* nil)
(defvar *names* (make-hash-table :test #'equal))
(defvar *viewport* nil)
(defvar *transform* +identity-4+)
(defvar *placement* +identity-4+)
(defvar *style* nil)
(defvar *style-list* (make-hash-table))

(defstruct elem-collector
  elements)


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

(defmacro with-placement (placement &body body)
  `(let ((*placement* (call-delayed #'mm-* *placement* ,placement))
         (*transform* +identity-4+))
     ,@body))

(defmacro with-at (at &body body)
  `(if ,at
       (with-placement (placement-transform ,at) ,@body)
       (progn ,@body)))

(defmacro with-style (style &rest body)
  `(let ((*style* (merge-style *style* ,style)))
     ,@body))

(defmacro with-viewport (v &rest body)
  `(let ((*viewport* ,v))
     ,@body))

(defmacro with-clip (drawing &rest body)
  `(progn
     (let ((newclip nil))
       (let ((*elements* (make-elem-collector))
             (*clip* nil))
         (progn ,@drawing)
         (setf newclip (intern-resource
                        (make-instance 'clip-resource
                          :elements (elem-collector-elements *elements*)))))
       (let ((*clip* newclip)) 
         ,@body))))


(defclass viewport ()
  ((view :initarg :view :reader viewport-view
         :type (array double-float (4 4))) ; 3D homogenous
   (proj :initarg :proj :reader viewport-proj
         :type (array double-float (4 4))) ; 3D homogenous
   (placement :initarg :placement :reader viewport-placement?)) ; 2D homogenous
  (:default-initargs :view +ortho-z+ :proj +ortho-z+ :placement +placement-identity+))

(defun viewport-placement (viewport)
  (resolve (viewport-placement? viewport)))


(defun make-page-viewport (&key (view +ortho-z+) (proj +ortho-z+)
                                (scale 1d0) (origin-x 0d0) (origin-y 0d0)
                                (flip-y t))
  (make-instance 'viewport
    :view view :proj proj
    :placement (mat-3-3 (list scale 0 origin-x)
                        (list 0 (if flip-y (- scale) scale) origin-y)
                        (list 0 0 1))))



(defun viewport-project (viewport vec)
  (mv-* (viewport-placement viewport)
        (clip->page
         (mv-* (mm-* (viewport-proj viewport) (viewport-view viewport))
               vec))))


(defun picture-placement (parent at along scale pivot flip-y)
  (let* ((at (resolve at))
         (along (resolve along))
         (origin (viewport-project parent at))
         (ang 0))
    (when along
      (let* ((tip (viewport-project parent
                                    (v+ at (scale-vec *page-frame-step-size* along))))
             (dx (- (vec-x tip) (vec-x origin)))
             (dy (- (vec-y tip) (vec-y origin)))
             (l (sqrt (+ (* dx dx) (* dy dy)))))
        (unless (< l +epsilon+) (setf ang (atan dy dx)))))
    (mm-* (mat-3-translate (vec-x origin) (vec-y origin))
          (mat-3-rot-z (rad->deg ang))
          (mat-3-3-d scale (if flip-y (- scale) scale) 1)
          (mat-3-translate (- (vec-x pivot)) (- (vec-y pivot))))))

(defun make-picture-viewport (&key at along (scale 1d0) (pivot (vec-2 0 0))
                                   (flip-y t) (parent *viewport*))
  (let ((d (delay (picture-placement parent at along scale pivot flip-y))))
    (push d *pending*)
    (make-instance 'viewport :view +ortho-z+ :proj +ortho-z+ :placement d)))

(defmacro with-page-picture ((&rest args) &body body)
  `(let ((*viewport* (make-picture-viewport ,@args))
         (*transform* +identity-4+)
         (*placement* +identity-4+))
     ,@body))



(defun resolve-element (elem? &optional viewport)
  (if (or (typep elem? 'element) (typep elem? 'delayed))
      elem?
      (resolve (or (gethash (cons elem?  viewport) *names*)
                   (gethash (cons elem?  *viewport*) *names*)
                   (error "Bad key")))))



(defun process (func &rest args)
  (let ((*elements* (make-elem-collector :elements nil))
        (*pending* nil)
        (*clip* nil)
        (*element-index* 0)
        (*resource-index* 0)
        (*resources* (make-hash-table :test #'equal))
        (*names* (make-hash-table :test #'equal))
        (*viewport* (or *viewport* (make-page-viewport)))
        (*transform* +identity-4+)
        (*placement* +identity-4+)
        (*style* nil))
    (apply func args)
    (loop while *pending* do
      (resolve (pop *pending*)))
    (list (nreverse (elem-collector-elements *elements*)) *resources*)))



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
        (pl *placement*)
        (c (or clip *clip*))
        (idx (incf *element-index*))
        (local-elements *elements*)
        (v (or viewport *viewport*)))
    (flet ((build-element (p)
             (let ((tr (ecase (primitive-space p)
                         (:local (or (resolve transform) (resolve tr1)))
                         (:world +identity-4+))))
               (make-instance 'element
                 :primitive p
                 :transform tr :placement (resolve pl)
                 :viewport v :style s :clip c
                 :index idx))))
      (if (or (deep-delayed-p primitive) (delayed-p transform) (delayed-p tr1) (delayed-p pl))
          (progn
            (setq element (delay
                           (first (push (build-element (deep-resolve primitive))
                                        (elem-collector-elements local-elements)))))
            (push element *pending*))
          (setq element (first (push (build-element primitive)
                                     (elem-collector-elements local-elements)))))
      (when name
        (setf (gethash (cons name v) *names*) element))
      element)))

