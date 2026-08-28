(in-package :quickdraw)


(defparameter *svg-coordinate-precision* 4)

(defclass svg-backend ()
  ((elements :accessor svg-backend-elements :initform nil)
   (defs     :accessor svg-backend-defs     :initform nil)
   (stream   :accessor svg-backend-stream   :initarg :stream :initform nil)
   (extents  :accessor svg-backend-extents   :initform nil)
   (padding  :accessor svg-backend-padding   :initform 10)
   (rendering-defs    :accessor svg-backend-rendering-defs   :initform nil)
   (width    :accessor svg-backend-width    :initarg :width  :initform 600)
   (height   :accessor svg-backend-height   :initarg :height :initform 600)))


(defmethod init-backend ((backend svg-backend)))

(defun svg-get-extents (backend)
  (loop for p in (svg-backend-extents backend)
        maximizing (vec-x p) into max-x
        minimizing (vec-x p) into min-x
        maximizing (vec-y p) into max-y
        minimizing (vec-y p) into min-y
        finally (return (values min-x min-y max-x max-y))))

(defun svg-resource-ref (resource)
  (format nil "url(#~a)" (resource-id resource)))

(defun style-val->string (value)
  (typecase value
    (resource  (svg-resource-ref value))
    (color  (color-css value))
    (float  (format nil "~,vf" *svg-coordinate-precision* value))
    (real  (format nil "~,vf" *svg-coordinate-precision* (coerce value 'double-float)))
    (string value)
    (symbol (string-downcase (symbol-name value)))
    (t (format nil "~a" value))))


(defun attr (key value)
  (list (string-downcase (string key))
        (style-val->string value)))


(defun resource-p (x)
  (typep x 'resource))


(defun style->list (style)
  (loop for (k v) on style
        by #'cddr
        collect (list (string-downcase (string k)) (style-val->string v))))


(defun matrix->svg-transform (mat)
  (format nil "matrix(~,2f ~,2f ~,2f ~,2f ~,2f ~,2f)"
          (aref mat 0 0) (aref mat 1 0)
          (aref mat 0 1) (aref mat 1 1)
          (aref mat 0 3) (aref mat 1 3)))


(defun svg-to-string (backend)
  (with-output-to-string (out)
    (xmls:write-xml
     (xmls:nodelist->node 
      `("svg" (("width" ,(format nil "~d" (svg-backend-width backend)))
               ("height" ,(format nil "~d" (svg-backend-height backend)))
               ("xmlns" "http://www.w3.org/2000/svg")
               ("viewBox" ,(multiple-value-bind (min-x min-y max-x max-y)
                               (svg-get-extents backend)
                             (let ((padding (svg-backend-padding backend)))
                               (format nil "~,vf ~,vf ~,vf ~,vf"
                                       *svg-coordinate-precision* (- min-x padding)
                                       *svg-coordinate-precision* (- min-y padding)
                                       *svg-coordinate-precision* (+ (- max-x min-x) (* 2 padding))
                                       *svg-coordinate-precision* (+ (- max-y min-y) (* 2 padding)))))))
              ,@(when (svg-backend-defs backend)
                  `(("defs" nil ,@(nreverse (svg-backend-defs backend)))))
              ,@(nreverse (svg-backend-elements backend))))
     out :indent 2)))

(defun point-to-pair (v)
  (format nil "~,2f ~,2f" (vec-x v) (vec-y v))) 

(defun points-to-tuple (&rest vecs)
  (format nil "~{~a~^, ~}" (mapcar #'point-to-pair vecs)))



(defun svg-maybe-wrap (element tag attrs &optional text)
  (let* ((node (if text
                   (list tag attrs text)
                   (list tag attrs)))
         (clip (element-clip element)))
    (if clip
        `("g" (("clip-path" ,(format nil "url(#~a)" (resource-id clip))))
              ,node)
        node)))



(defun emit-projected-points (element tag points style)
  (let* ((trans (elem->placement-func element))
         (p (mapcar (lambda (p) (point-to-pair (funcall trans p))) points)))
    (svg-maybe-wrap element
                    tag
                    (append
                     (list (attr "points" (format nil "~{~a~^ ~}" p)))
                     (style->list style)))))


(defun render-sampled (primitive element)
  (emit-projected-points element
                         "polyline" (primitive-sample primitive) (element-style element)))

(defmethod render-primitive :around ((backend svg-backend) (p primitive) element)
  (let ((e (if (element-exact-p element)
               (call-next-method)
               (render-sampled p element))))
    (if (svg-backend-rendering-defs backend)
        e
        (progn
          (push e (svg-backend-elements backend))
          (let ((transform (elem->placement-func element)))
            (loop for p in (primitive-extents (element-primitive element))
                  do (push (funcall transform p)
                           (svg-backend-extents backend))))))))




(defmethod render-primitive ((backend svg-backend) (p face) element)
  (let* ((side (primitive-face-side p element))
         (style (if (and (eql side :back) (back-style p))
                    (back-style p)
                    (element-style element))))
    (emit-projected-points element "polygon" (points p) style)))

(defmethod render-primitive ((backend svg-backend) (p polyline) element)
  (emit-projected-points element "polyline" (points p)
                         (element-style element)))

(defmethod render-primitive ((backend svg-backend) (p segment) element)
  (let* ((trans (elem->placement-func element))
         (p0 (funcall trans (start p)))
         (p1 (funcall trans (end p))))
    (svg-maybe-wrap element "line"
                    (append (list (attr "x1" (vec-x p0)) (attr "y1" (vec-y p0))
                                  (attr "x2" (vec-x p1)) (attr "y2" (vec-y p1)))
                            (style->list (element-style element))))))

(defmethod render-primitive ((backend svg-backend) (p rect) element)
  (svg-maybe-wrap element "rect"
                  (append (list (attr "x" (- (/ (w p) 2)))
                                (attr "y" (- (/ (h p) 2)))
                                (attr "width" (w p))
                                (attr "height" (h p))
                                (attr "rx" (rx p))
                                (attr "ry" (ry p))
                                (attr "transform"
                                      (matrix->svg-transform
                                       (element->placement-mat element))))
                          (style->list (element-style element)))))

(defmethod render-primitive ((backend svg-backend) (p circle) element)
  (svg-maybe-wrap element "circle"
                  (append (list (attr "cx" 0) (attr "cy" 0) (attr "r" (r p))
                                (attr "transform"
                                      (matrix->svg-transform
                                       (element->placement-mat element))))
                          (style->list (element-style element)))))

(defmethod render-primitive ((backend svg-backend) (p ellipse) element)
  (svg-maybe-wrap element "ellipse"
                  (append (list (attr "cx" 0) (attr "cy" 0)
                                (attr "rx" (rx p)) (attr "ry" (ry p))
                                (attr "transform"
                                      (matrix->svg-transform
                                       (element->placement-mat element))))
                          (style->list (element-style element)))))

(defmethod render-primitive ((backend svg-backend) (p path) element)
  (let* ((commands (path->svg-commands (geometry p)))
         (trans (elem->placement-func element))
         (d nil))
    (loop while commands do
      (let ((cmd (pop commands)))
        (ecase cmd
          (:M (let ((x (pop commands)))
                (push (format nil "M ~a" (point-to-pair (funcall trans x))) d)))
          (:L (let ((x (pop commands)))
                (push (format nil "L ~a" (point-to-pair (funcall trans x))) d)))
          (:C (let ((p1 (pop commands)) (p2 (pop commands)) (p3 (pop commands)))
                (push (format nil "C ~a"
                              (points-to-tuple (funcall trans p1)
                                               (funcall trans p2)
                                               (funcall trans p3)))
                      d)))
          (:Q (let ((p1 (pop commands)) (p2 (pop commands)))
                (push (format nil "Q ~a"
                              (points-to-tuple (funcall trans p1)
                                               (funcall trans p2)))
                      d)))
          (:Z (push "Z" d)))))
    (svg-maybe-wrap element "path"
                    (append (list (attr "d" (format nil "~{~a~^ ~}" (nreverse d))))
                            (style->list (element-style element))))))

(defmethod render-primitive ((backend svg-backend) (p label) element)
  (let ((pt (elem->placement element (vec-4 0 0 0 1))))
    (svg-maybe-wrap element "text"
                    (append (list (attr "x" (vec-x pt))
                                  (attr "y" (vec-y pt))
                                  (attr "text-anchor"
                                        (ecase (align p)
                                          (:center "middle")
                                          (:left "start")
                                          (:right "end")))
                                  (attr "dominant-baseline"
                                        (ecase (baseline p)
                                          (:middle "middle")
                                          (:top "hanging")
                                          (:bottom "auto"))))
                            (style->list (element-style element)))
                    (text p))))




(defmethod render-resource ((backend svg-backend) (resource clip-resource))
  (setf (svg-backend-rendering-defs backend) t)
  (let ((clip-id (resource-id resource))
        (elems (loop for e in (clip-elements resource)
                     collect
                     (render-primitive backend (element-primitive e)  e))))
    (push `("clipPath" (("id" ,clip-id))
                       ,@elems)
          (svg-backend-defs backend)))
  (setf (svg-backend-rendering-defs backend) nil))



(defmethod render-resource ((backend svg-backend) (resource linear-gradient-resource))
  (let ((clip-id (resource-id resource)))
    (push `("linearGradient"
            (("id" ,clip-id)
             ("x1" 0)
             ("y1" 0)
             ("x2" ,(format nil "~,2f" (vec-x (gradient-direction resource))))
             ("y2" ,(format nil "~,2f" (vec-y (gradient-direction resource)))))
            ,@(loop for stop in (gradient-stops resource)
                    collect
                    `("stop"
                      (("offset" ,(format nil "~d%" (floor (* 100 (car stop)))))
                       ("stop-color"  ,(color-css (cdr stop)))))))
          (svg-backend-defs backend))))

