(in-package :clikz)

(defclass element ()
  ((primitive :initarg :primitive :reader element-primitive)
   (transform :initarg :transform :reader element-transform) ; How the element is tranformed in model space
   (placement :initarg :placement :reader element-placement :initform +identity-4+) ; How the element should is placed relative to its parent frame
   (viewport :initarg :viewport :reader element-viewport)
   (style :initarg :style :reader element-style)
   (clip :initarg :clip :reader element-clip)
   (layer :initarg :layer :reader element-layer :initform 0)
   (index :initarg :index :accessor element-index :initform 0)))


;; The transform chain looks like
;; model -> transform -> placement -> clip -> project -> page-placement

(defun elem->world (elem)
  (mm-* (element-placement elem)
        (element-transform elem)))

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


(defun element-anchor-point (element key-or-ang &rest args)
  (if (and (numberp key-or-ang) (null args))
      (element-boundary-point
       element
       (vec-dir (cos (deg->rad key-or-ang)) (sin (deg->rad key-or-ang)) 0))
      (apply #'primitive-anchor (element-primitive element) key-or-ang args)))

(defun element-boundary-point (element direction)
  (primitive-boundary (element-primitive element) direction))


(defmethod primitive-face-side ((p primitive) element)
  (declare (ignore element))
  nil)

(defmethod primitive-cull-p ((p primitive) element)
  (declare (ignore element)) nil)


(defun clip->page (vec)
  "Standard opengl like de-homogenization"
  (let ((w (vec-w vec)))
    (vec-3 (/ (vec-x vec) w) (/ (vec-y vec) w) 1)))


