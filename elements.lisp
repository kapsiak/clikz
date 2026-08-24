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

(defun emit (type params
             &key transform viewport style clip name  anchor boundary)
  (let (element
        (s (or style *style*))
        (tr (or transform *transform*))
        (c (or clip *clip*))
        (idx (incf *element-index*))
        (v (or viewport *viewport*)))
    (flet ((build-element (params)
             (make-instance 'element
               :type type
               :transform tr :viewport v :style  s :clip c
               :anchor anchor :boundary boundary
               :index idx :params (deep-resolve params))))
      (if (deep-delayed-p params)
          (progn
            (setq element (delay (first (push (build-element params) *elements*))))
            (push element *pending*))
          (progn
            (setq element (first (push (build-element params) *elements*)))
            (push element *elements*)))
      (when name
        (setf (gethash (cons name v) *names*) element)))))


(defun emit-absolute (type params &key style clip name anchor boundary)
  (emit type params
        :transform +identity-4+
        :style (or style *style*) :clip (or clip *clip*)
        :name name :anchor anchor :boundary boundary))

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
      (if (> nz 0) :front :back))))

(defun clip->page (vec)
  (let ((w (vec-w vec)))
    (vec-3 (/ (vec-x vec) w) (/ (vec-y vec) w) 1)))

(defun getf-elem (elem key)
  (getf (element-params elem) key))

