(in-package :quickdraw)

(declaim (optimize (speed 0) (space 0) (safety 3) (debug 3)))

(defvar *elements* nil)
(defvar *pending* nil)
(defvar *clip* nil)
(defvar *resources* (make-hash-table :test #'equal))
(defvar *names* (make-hash-table :test #'equal))
(defvar *viewport* nil)
(defvar *current-drawing-dim* 3)
(defvar *transform* +identity-4+)
(defvar *style* nil)

(defun merge-style (orig updates)
  (let (( ret (copy-tree  orig)))
    (loop for (key value) on updates by #'cddr
          do
             (setf (getf ret key) value))
    ret))


(defmacro with-transform (transform &rest body)
  `(let ((*transform* (mm-* ,transform *transform* ))
         (*current-drawing-dim* (array-dimension ,transform 1)))
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


(defun p (&rest coords)
  (ecase *current-drawing-dim*
    (2 (ecase (length coords)
         (2 (vec-3 (first coords) (second coords) 1))))
    (3
     (ecase (length coords)
       (2 (vec-4 (first coords) (second coords) 0 1))
       (3 (vec-4 (first coords) (second coords) (third coords) 1))))
    (4
     (ecase (length coords)
       (2 (vec-4 (first coords) (second coords) 0 1))
       (3 (vec-4 (first coords) (second coords) (third coords) 1))))))

;; 4->3
(defun clip->page (vec)
  (let ((w (vec-w vec)))
    (vec-3 (/ (vec-x vec) w) (/ (vec-y vec) w) 1)))




(defun elem->eye (elem)
  (mm-*
   (viewport-view (element-viewport elem))
   (element-transform elem)))

(defun elem->clip (elem)
  (m-4-*
   (viewport-proj (element-viewport elem))
   (elem->eye elem)))


(defun elem->placement (elem vec)
  (let ((ecm (elem->clip elem))
        (place (viewport-placement (element-viewport elem))))
    (mv-3-*
     place
     (clip->page (mv-* ecm vec)) )))

(defun elem->placement-func (elem)
  (let ((ecm (elem->clip elem))
        (place (viewport-placement (element-viewport elem))))
    (lambda (vec)
      (mv-3-*
       place
       (clip->page (mv-* ecm vec)) ))))


(defun element-affine-p (elem)
  (affine-p (elem->clip elem)))


(defun element->placement-mat (elem)
  (let* ((vp (element-viewport elem)))
    (mm-* (viewport-placement vp) +drop-z+ (elem->clip elem))))

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


(defun getf-elem (elem key)
  (getf (element-params elem) key))

(defclass resource ()
  ((id :initarg :id :reader resource-id)))

(defun emit (type &rest params
                  &key transform viewport style clip name  anchor boundary
                  &allow-other-keys)
  (let (element
        (s (or style *style*))
        (tr (or transform *transform*))
        (c (or clip *clip*))
        (v (or viewport *viewport*)))
    
    (if (deep-delayed-p params)
        (progn 
          (setq element (delay
                         (with-transform tr
                           (let ((r (make-instance 'element
                                      :type type
                                      :transform tr :viewport v :style  s :clip c
                                      :anchor anchor :boundary boundary
                                      :params (deep-resolve params))))
                             (push r *elements*)
                             r))))
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

(defun resolve-name (name &optional viewport)
  (resolve (or (gethash (cons name  viewport) *names*)
               (gethash (cons name  *viewport*) *names*)
               (error "Bad key"))))


(defun elem-depth (elem)
  (mv-* (elem->eye elem)
        (element-centroid (element-type elem) (element-params elem))))
