(in-package :quickdraw)


(defun elem->eye (elem)
  (mm-*
   (viewport-view (element-viewport elem))
   (element-transform elem)))

(defun elem->clip (elem)
  (mm-*
   (viewport-proj (element-viewport elem))
   (elem->eye elem)))


(defun elem->placement (elem vec)
  (let ((ecm (elem->clip elem))
        (place (viewport-placement (element-viewport elem))))
    (mv-*
     place
     (clip->page (mv-* ecm vec)) )))

(defun elem->placement-func (elem)
  (let ((ecm (elem->clip elem))
        (place (viewport-placement (element-viewport elem))))
    (lambda (vec)
      (mv-*
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
   (index :initarg :index :accessor element-index :initform 0)
   (boundary :initarg :boundary :reader element-boundary)))


(defun element-centroid (element)
  (primitive-centroid (element-type element) (element-params element)))

(defun element-centroid-eye (element)
  (mv-* (elem->eye element) (element-centroid element)))

(defun element-depth (element)
  (vec-z (element-centroid-eye element)))

(defun element-face-side (element)
  (when (eql (element-type element) :face)
    (let* ((n (getf (element-params element) :normal))
           (eye (elem->eye element))
           (nz (vec-z (mv-* eye (vec-4 (vec-x n) (vec-y n) (vec-z n) 0)))))
      (if (< nz 0) :front :back))))

(defun clip->page (vec)
  (let ((w (vec-w vec)))
    (vec-3 (/ (vec-x vec) w) (/ (vec-y vec) w) 1)))

(defun getf-elem (elem key)
  (getf (element-params elem) key))

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
