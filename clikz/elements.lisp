(in-package :clikz)

;;;; An element is a primitive plus the ambient context captured when it was
(defclass element ()
  ((primitive :initarg :primitive :reader element-primitive)
   (transform :initarg :transform :reader element-transform)
   (placement :initarg :placement :reader element-placement :initform +identity-4+)
   (viewport :initarg :viewport :reader element-viewport)
   (style :initarg :style :reader element-style)
   (clip :initarg :clip :reader element-clip)
   (index :initarg :index :accessor element-index :initform 0)))

(defun elem->world (elem)
  (mm-* (element-placement elem) (element-transform elem)))

(defun elem->eye (elem)
  (mm-*
   (viewport-view (element-viewport elem))
   (elem->world elem)))

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


(defun element-exact-p (elem)
  (or (eq (primitive-exact-under (element-primitive elem)) :any)
      (element-affine-p elem)))


(defun element-centroid (element)
  (primitive-centroid (element-primitive element)))

(defun element-centroid-eye (element)
  (mv-* (elem->eye element) (element-centroid element)))

(defun element-depth (element)
  (vec-z (element-centroid-eye element)))


(defun element-anchor-point (element key &rest args)
  (apply #'primitive-anchor (element-primitive element) key args))

(defun element-boundary-point (element direction)
  (primitive-boundary (element-primitive element) direction))


(defmethod primitive-face-side ((p primitive) element)
  (declare (ignore element))
  nil)

(defmethod primitive-cull-p ((p primitive) element)
  (declare (ignore element)) nil)


(defun clip->page (vec)
  (let ((w (vec-w vec)))
    (vec-3 (/ (vec-x vec) w) (/ (vec-y vec) w) 1)))


