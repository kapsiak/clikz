(in-package :quickdraw)

(declaim (optimize (speed 0) (space 0) (safety 3) (debug 3)))

(defvar *elements* nil)
(defvar *pending* nil)
(defvar *clip* nil)
(defvar *resources* (make-hash-table :test #'equal))
(defvar *names* (make-hash-table :test #'equal))
(defvar *viewport* nil)
(defvar *transform* nil)
(defvar *style* nil)


(defparameter +page-identity+ (mat-2-4 (list 1 0 0 0)
                                       (list 0 1 0 0)))

(defparameter +ortho-z+ (mat-4 (list 1 0 0 0)
                               (list 0 1 0 0)
                               (list 0 0 1 0)
                               (list 0 0 0 1)))

(defclass viewport ()
  ((view :initarg :view :reader viewport-view
         :type (array double-float (4 4)))
   (page :initarg :page :reader viewport-page
         :type (array double-float (2 4))))
  (:default-initargs :view +ortho-z+ :page +page-identity+))





(defun merge-style (orig updates)
  (let (( ret (copy-tree  orig)))
    (loop for (key value) on updates by #'cddr
          do
             (setf (getf ret key) value))
    ret))




(defclass element ()
  ((type :initarg :type :reader element-type)
   (transform :initarg :transform :reader element-transform)
   (viewport :initarg :viewport :reader element-viewport)
   (params :initarg :params :reader element-params)
   (style :initarg :style :reader element-style)
   (clip :initarg :clip :reader element-clip)
   (anchor :initarg :anchor :reader element-anchor)
   (boundary :initarg :boundary :reader element-boundary)))

(defmethod print-object ((obj viewport) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (obj stream :type t :identity t)
        (format stream ":VIEW ~S :PAGE ~S"
                (if (slot-boundp obj 'view) (viewport-view obj) :unbound)
                (if (slot-boundp obj 'page) (viewport-page obj) :unbound)))))

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



(defun getf-elem (elem key)
  (getf elem key))


(defclass resource ()
  ((id :initarg :id :reader resource-id)))


(defclass clip (resource) ())


(defun emit (type &rest params
                  &key transform viewport style clip name  anchor boundary
                  &allow-other-keys)
  (let (element
        (s (or style *style*))
        (tr (or transform *transform*))
        (c (or clip *clip*))
        (v (or viewport *viewport*)))
    
    (if (some #'delayed-p params)
        (progn 
          (setq element (delay
                         (let ((r (make-instance 'element
                                    :type type
                                    :transform tr :viewport v :style  s :clip c
                                    :anchor anchor :boundary boundary
                                    :params (mapcar #'resolve params))))
                           (push r *elements*)
                           r)))
          (push element *pending*))
        (progn 
          (setq element 
                (make-instance 'element
                  :type type
                  :transform tr :viewport v :style  s :clip c
                  :anchor anchor :boundary boundary
                  :params params))
          (push element *elements*)))
    (when name
      (setf (gethash (cons name v) *names*) element))))







(defun resolution ()
  (loop while *pending* do
    (resolve (pop *pending*))))


(defun process (func)
  (let ((*elements* nil)
        (*pending* nil)
        (*clip* nil)
        (*resources* (make-hash-table :test #'equal))
        (*names* (make-hash-table :test #'equal))
        (*viewport* (make-instance 'viewport))
        (*transform* (mat-4-d 1 1 1 1))
        (*style* nil))
    (funcall func)
    (loop for key being each hash-key of *names*
            using (hash-value value)
          do (format t "Key: ~S, Value: ~S~%" key value))


    (resolution)
    (list *elements* *resources*)))


(defmacro with-transform (transform &rest body)
  `(let ((*transform* ,transform))
     ,@body))

(defmacro with-style (style &rest body)
  `(let ((*style* ,(merge-style *style* style)))
     ,@body))

(defmacro with-viewport (v &rest body)
  `(let ((*viewport* ,v))
     ,@body))

(defun elem->eye (elem)
  (m-4-*
   (viewport-view (element-viewport elem))
   (element-transform elem)))

(defun elem->page (elem)
  (m-2-4-*
   (viewport-page (element-viewport elem))
   (elem->eye elem)))


(defun resolve-name (name &optional viewport)
  (resolve (or (gethash (cons name  viewport) *names*)
               (gethash (cons name  *viewport*) *names*)
               (error "Bad key"))))

(defun at (name anchor &optional (viewport *viewport*) )
  (delay
   (let* ((e (resolve-name name viewport))
          (p (funcall (element-anchor e) anchor)))
     (mv-4-* (element-transform e) p))))


(defun page-at (name anchor  &optional (viewport *viewport*) )
  (delay
   (let* ((e (resolve-name name viewport))
          (tr (elem->page e)))
     (mv-2-4-* tr
               (funcall (element-anchor e) anchor)))))


(defun is-page-point (vec)
  (= (array-dimension vec 0) 2))

(defun test2 ()
  (with-viewport
      (make-instance 'viewport :view (mat-4-rot-x 30))
    (emit :segment :start (delay 2)
                   :end (vec-4 1 1 0 1)
                   :name 's1
                   :anchor (lambda (x)
                             (ecase x
                               (:end (vec-4 40 1 0 1))
                               (:start (vec-4 -25 1 0 1))
                               )))
    (emit :segment :start (at 's1 :end)
                   :end (page-at 's1 :start))
    ))

(process #'test2)





