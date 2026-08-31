(in-package :clikz)

(defvar *math-size* 10d0)
(defvar *math-cache-dir* (uiop:ensure-directory-pathname (merge-pathnames "clikzmath" (uiop:temporary-directory))))
(defvar *latex-program* "latex")
(defvar *dvisvgm-program* "dvisvgm")

(defvar *text->svg-func* nil)

(defvar *latex-doc-class* "article")
(defvar *latex-packages* '(("amsmath" . nil)
                           ("amssymb" . nil)
                           ("preview" . ("active" "tightpage" "textmath" "displaymath"))))

(defvar *typst-program* "typst")
(defvar *typst-template* "
#set page(width: auto, height: auto, margin: 0pt, fill: none)
#import \"@preview/mitex:0.2.7\": *
#mitex(`
~a
`)")

(defstruct text-box paths width height depth)

(define-primitive glyphs (:world) paths)

(defmethod primitive-sample ((p glyphs) &key (steps 8))
  (loop for c in (paths p) append (path-resample c steps)))

(defmethod primitive-centroid ((p glyphs))
  (centroid-of-points (primitive-sample p :steps 2)))


(defun draw-math (text &key (size 10d0) name style)
  (let ((box (text-box text)))
    (with-transform (mat-4-scale size size size)
      (emit (make-instance 'glyphs
              :paths (mapcar (lambda (c) (call-path-points #'p c))
                             (text-box-paths box)))
            :name name :style (merge-style *style* style)))))


(defun render-packages (packages)
  (flet ((render-package (package)
           (format nil "\\usepackage[~{~a~^,~}]{~a}" (cdr package) (car package))))
    (format nil "~{~a~%~}" (mapcar #'render-package packages))))

(defun build-latex-preamble (&optional (class *latex-doc-class*) (packages *latex-packages*))
  (format nil "\\documentclass{~a}~%~a" class (render-packages packages)))


(defun build-document (text
                       &optional (class *latex-doc-class*) (packages *latex-packages*))
  (format nil "~a~%\\begin{document}~%~a~%\\end{document}~%"
          (build-latex-preamble class packages)
          text))

(defun text->svg-latex (text)
  (let* ((doc (build-document text))
         (hash (hash-string doc))
         (path (merge-pathnames (format nil "~d.svg" hash) *math-cache-dir*)))
    (ensure-directories-exist path)
    (unless (uiop:file-exists-p path)
      (with-temporary-directory (dir)
        (with-open-file (s (merge-pathnames "rendered_math.tex" dir)
                           :direction :output)
          (write-string doc s))
        (run-external *latex-program*
                      '("-interaction=nonstopmode" "-halt-on-error" "rendered_math.tex")
                      :directory dir)
        (run-external *dvisvgm-program*
                      `("--no-fonts" "--exact-bbox" "-o" ,path "rendered_math.dvi")
                      :directory dir)))
    (uiop:read-file-string path)))

(defun text->svg-typst (text)
  (let* ((doc (format nil *typst-template* text))
         (hash (hash-string doc))
         (path (merge-pathnames (format nil "~d.svg" hash) *math-cache-dir*)))
    (ensure-directories-exist path)
    (unless (uiop:file-exists-p path)
      (with-temporary-directory (dir)
        (with-open-file (s (merge-pathnames "rendered_math.typ" dir)
                           :direction :output)
          (write-string doc s))
        (run-external *typst-program* (list "compile" "rendered_math.typ" "rendered_math.dvi")
                      :directory dir)
        (run-external *dvisvgm-program*
                      `("--no-fonts" "--exact-bbox" "-o" ,path "rendered_math.dvi")
                      :directory dir)))
    (uiop:read-file-string path)))


(defun math-rect-path (x y w h)
  (path-from-points (list (vec-p x (- y))
                          (vec-p (+ x w) (- y))
                          (vec-p (+ x w) (- (+ y h)))
                          (vec-p x (- (+ y h))))
                    :closed t))

(defun math-parse-svg (svg)
  (let* ((root (xmls:parse svg))
         (scale (/ 1d0 *math-size*))
         (outlines (make-hash-table :test #'equal))
         (paths nil))

    (labels ((collect-defs (node)
               (when (equal (xmls:node-name node) "path")
                 (setf (gethash (xmls:xmlrep-attrib-value "id" node) outlines)
                       (parse-svg-path (xmls:xmlrep-attrib-value "d" node))))
               (mapc #'collect-defs (xmls:node-children node)))
             (walk (node)
               (let ((tag (xmls:node-name node)))
                 (cond
                   ((equal tag "defs") (collect-defs node))
                   ((equal tag "use")
                    (let ((id (subseq (xmls:xmlrep-attrib-value "href" node) 1))
                          (x (parse-svg-num (xmls:xmlrep-attrib-value "x" node)))
                          (y (parse-svg-num (xmls:xmlrep-attrib-value "y" node))))
                      (dolist (p (gethash id outlines))
                        (push (call-path-points
                               (lambda (vec)
                                 (vec-p (+ (vec-x vec) x)
                                        (- (+ (vec-y vec) y))))
                               p)
                              paths))))
                   ((equal tag "rect")
                    (push (math-rect-path (parse-svg-num (xmls:xmlrep-attrib-value "x" node))
                                          (parse-svg-num (xmls:xmlrep-attrib-value "y" node))
                                          (parse-svg-num (xmls:xmlrep-attrib-value "width" node))
                                          (parse-svg-num (xmls:xmlrep-attrib-value "height" node)))
                          paths))
                   (t (mapc #'walk (xmls:node-children node)))))))
      (walk root))
    (destructuring-bind (min-x min-y w h)
        (esrap:parse 'svg-2-coords (xmls:xmlrep-attrib-value "viewBox" root))
      (declare (ignore min-x))
      (make-text-box
       :paths (mapcar (lambda (p)
                        (call-path-points
                         (lambda (v) (vec-p (* scale (vec-x v))
                                            (* scale (vec-y v))))
                         p))
                      (nreverse paths))
       :width  (* scale w)
       :height (* scale (- min-y))
       :depth  (* scale (+ min-y h))))))


(setf *text->svg-func* #'text->svg-latex)

(defun text-box (text)
  (math-parse-svg (funcall *text->svg-func* text)))
