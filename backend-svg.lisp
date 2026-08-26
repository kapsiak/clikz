(in-package :quickdraw)


(defparameter *svg-coordinate-precision* 4)

(defclass svg-backend ()
  ((elements :accessor svg-backend-elements :initform nil)
   (defs     :accessor svg-backend-defs     :initform nil)
   (stream   :accessor svg-backend-stream   :initarg :stream :initform nil)
   (fit-extents :accessor svg-backend-fit-extents   :initarg :fit-extents :initform t)
   (width    :accessor svg-backend-width    :initarg :width  :initform 600)
   (height   :accessor svg-backend-height   :initarg :height :initform 600)))


(defmethod init-backend ((backend svg-backend)))

(defmethod supports-p ((backend svg-backend) feature)
  (member feature '(:segment :polyline :face :rect :circle :ellipse
                    :path :label :linear-gradient :radial-gradient
                    :hatch :clip :marker :math)))

(defun svg-add (backend element)
  (push element (svg-backend-elements backend)))

(defun svg-add-def (backend element)
  (push element (svg-backend-defs backend)))


(defun style-val->string (value)
  (typecase value
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
               ("xmlns" "http://www.w3.org/2000/svg"))
              ,@(when (svg-backend-defs backend)
                  `(("defs" nil ,@(nreverse (svg-backend-defs backend)))))
              ,@(nreverse (svg-backend-elements backend))))
     out :indent 2)))

(defun point-to-pair (v)
  (format nil "~,2f ~,2f" (vec-x v) (vec-y v))) 

(defun points-to-tuple (&rest vecs)
  (format nil "~{~a~^, ~}" (mapcar #'point-to-pair vecs)))



(defun svg-emit (backend element tag attrs &optional text)
  (let* ((node (if text
                   (list tag attrs text)
                   (list tag attrs)))
         (clip (element-clip element)))
    (svg-add backend
             (if clip
                 `("g" (("clip-path" ,(format nil "url(#~a)" (resource-id clip))))
                       ,node)
                 node))))


(defun render-projected-polygon (backend element tag style)
  (let* ((params (element-params element))
         (trans (elem->placement-func element))
         (pts (mapcar (lambda (p) (point-to-pair (funcall trans p)))
                      (getf params :points))))
    (svg-emit backend element tag
              (append (list (attr "points" (format nil "~{~a~^ ~}" pts)))
                      (style->list style)))))


(defmethod render-element ((backend svg-backend) (type (eql :face)) element)
  (let* ((side (element-face-side element))
         (back-style (getf (element-params element) :back-style))
         (style (if (and (eql side :back) back-style)
                    back-style
                    (element-style element))))
    (render-projected-polygon backend element "polygon" style)))

(defmethod render-element ((backend svg-backend) (type (eql :polyline)) element)
  (render-projected-polygon backend element "polyline"
                            (element-style element)))


(defmethod render-element ((backend svg-backend) (type (eql :segment)) element)
  (let* ((params (element-params element))
         (trans (elem->placement-func element))
         (p0 (funcall trans (getf params :start)))
         (p1 (funcall trans (getf params :end))))
    (svg-emit backend element "line"
              (append (list (attr "x1" (vec-x p0)) (attr "y1" (vec-y p0))
                            (attr "x2" (vec-x p1)) (attr "y2" (vec-y p1)))
                      (style->list (element-style element))))))


(defmethod render-element ((backend svg-backend) (type (eql :rect)) element)
  (let* ((params (element-params element))
         (w (getf params :w)) (h (getf params :h))
         (rx (or (getf params :rx) 0)) (ry (or (getf params :ry) 0)))
    (if (element-affine-p element)
        (svg-emit backend element "rect"
                  (append (list (attr "x" (- (/ w 2)))
                                (attr "y" (- (/ h 2)))
                                (attr "width" w)
                                (attr "height" h)
                                (attr "rx" rx)
                                (attr "ry" ry)
                                (attr "transform"
                                      (matrix->svg-transform
                                       (element->placement-mat element))))
                          (style->list (element-style element))))
        (let* ((trans (elem->placement-func element))
               (pts (mapcar (lambda (c) (point-to-pair (funcall trans c)))
                            (list (vec-4 (- (/ w 2)) (- (/ h 2)) 0 1)
                                  (vec-4 (/ w 2) (- (/ h 2)) 0 1)
                                  (vec-4 (/ w 2) (/ h 2) 0 1)
                                  (vec-4 (- (/ w 2)) (/ h 2) 0 1)))))
          (svg-emit backend element "polygon"
                    (append (list (attr "points"
                                        (format nil "~{~a~^ ~}" pts)))
                            (style->list (element-style element))))))))

(defmethod render-element ((backend svg-backend) (type (eql :circle)) element)
  (let ((r (getf (element-params element) :r)))
    (if (element-affine-p element)
        (svg-emit backend element "circle"
                  (append (list (attr "cx" 0)
                                (attr "cy" 0)
                                (attr "r" r)
                                (attr "transform"
                                      (matrix->svg-transform
                                       (element->placement-mat element))))
                          (style->list (element-style element))))
        (let* ((trans (elem->placement-func element))
               (pts (loop for i below 32
                          for th = (* 2 pi (/ i 32))
                          collect (point-to-pair
                                   (funcall trans
                                            (vec-4 (* r (cos th))
                                                   (* r (sin th)) 0 1))))))
          (svg-emit backend element "polygon"
                    (append (list (attr "points"
                                        (format nil "~{~a~^ ~}" pts)))
                            (style->list (element-style element))))))))

(defmethod render-element ((backend svg-backend) (type (eql :ellipse)) element)
  (let* ((params (element-params element))
         (rx (getf params :rx)) (ry (getf params :ry)))
    (if (element-affine-p element)
        (svg-emit backend element "ellipse"
                  (append (list (attr "cx" 0) (attr "cy" 0)
                                (attr "rx" rx) (attr "ry" ry)
                                (attr "transform"
                                      (matrix->svg-transform
                                       (element->placement-mat element))))
                          (style->list (element-style element))))
        (let* ((trans (elem->placement-func element))
               (pts (loop for i below 32
                          for th = (* 2 pi (/ i 32))
                          collect (point-to-pair
                                   (funcall trans
                                            (vec-4 (* rx (cos th))
                                                   (* ry (sin th)) 0 1))))))
          (svg-emit backend element "polygon"
                    (append (list (attr "points"
                                        (format nil "~{~a~^ ~}" pts)))
                            (style->list (element-style element))))))))



(defmethod render-element ((backend svg-backend) (type (eql :path)) element)
  (let* ((commands (path->svg-commands (getf-elem element :path)))
         (trans (elem->placement-func element))
         (d nil))
    (loop while commands do
      (let ((cmd (pop commands)))
        (ecase cmd
          (:M (let ((p (pop commands)))
                (push (format nil "M ~a" (point-to-pair (funcall trans p))) d)))
          (:L (let ((p (pop commands)))
                (push (format nil "L ~a" (point-to-pair (funcall trans p))) d)))
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

    (svg-emit backend element "path"
              (append (list (attr "d" (format nil "~{~a~^ ~}" (nreverse d))))
                      (style->list (element-style element))))))


(defmethod render-element ((backend svg-backend) (type (eql :label)) element)
  (let ((pt (elem->placement element (vec-4 0 0 0 1))))
    (svg-emit backend element "text"
              (append (list (attr "x" (vec-x pt))
                            (attr "y" (vec-y pt))
                            (attr "text-anchor"
                                  (ecase (getf-elem element :align)
                                    (:center "middle") (:left "start") (:right "end")))
                            (attr "dominant-baseline"
                                  (ecase (getf-elem element :baseline)
                                    (:middle "middle") (:top "hanging")
                                    (:bottom "auto"))))
                      (style->list (element-style element)))
              (getf-elem element :text))))


(defmethod resource-ref ((backend svg-backend) (resource resource))
  (format nil "url(#~a)" (resource-id resource)))
