(in-package :quickdraw)

(defvar *elements* nil)
(defvar *element-index* nil)
(defvar *resource-index* nil)
(defvar *pending* nil)
(defvar *clip* nil)
(defvar *element-index* nil)
(defvar *resources* (make-hash-table :test #'equal))
(defvar *names* (make-hash-table :test #'equal))
(defvar *viewport* nil)
(defvar *transform* +identity-4+)
(defvar *style* nil)
(defvar *path-length-cache* (make-hash-table))

(defun merge-style (orig updates)
  (let (( ret (copy-tree  orig)))
    (loop for (key value) on updates by #'cddr
          do
             (setf (getf ret key) value))
    ret))

(defmacro with-transform (transform &rest body)
  `(let ((*transform* (mm-* ,transform *transform* )))
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

(defmethod print-object ((obj element) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (obj stream :type t :identity t)
        (format stream "~A :TRANSFORM ~S :VIEWPORT ~S :PARAMS ~S :STYLE ~S :CLIP ~S :ANCHOR ~S :BOUNDARY ~S"
                (if (slot-boundp obj 'type) (element-type obj) :unbound)
                (if (slot-boundp obj 'transform) (element-transform obj) :unbound)
                (if (slot-boundp obj 'viewport) (element-viewport obj) :unbound)
                (if (slot-boundp obj 'params) (element-params obj) :unbound)
                (if (slot-boundp obj 'style) (element-style obj) :unbound)
                (if (slot-boundp obj 'clip) (element-clip obj) :unbound)
                (if (slot-boundp obj 'anchor) (element-anchor obj) :unbound)
                (if (slot-boundp obj 'boundary) (element-boundary obj) :unbound)))))


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
        (*viewport* (or *viewport* (make-instance 'viewport)))
        (*transform* +identity-4+)
        (*style* nil))
    (apply func args)
    (loop for key being each hash-key of *names*
            using (hash-value value)
          do (format t "Key: ~S, Value: ~S~%" key value))

    (loop while *pending* do
      (resolve (pop *pending*)))
    (list (nreverse *elements*) *resources*)))

