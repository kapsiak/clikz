(in-package :clikz)

(defvar *pt-to-world* 10d0)
(defvar *math-cache-dir* (uiop:ensure-directory-pathname (merge-pathnames "clikzmath" (uiop:temporary-directory))))
(defvar *latex-program* "latex")
(defvar *dvisvgm-program* "dvisvgm")
(defvar *text->svg-func* nil)
(defvar *latex-doc-class* "article")
(defvar *latex-packages* '(("fontenc" . ("T1"))
                           ("amsmath" . nil)
                           ("amssymb" . nil)
                           ("preview" . ("active" "tightpage" "auctex"))))

(defconstant +tex-points-per-point+ 65536d0)

(define-primitive text (:local)
  paths
  (text-str :initform nil)
  (width :initform 0d0)
  (height :initform 0d0)
  (depth :initform 0d0))


(defmethod primitive-sample ((p text) &key (steps 30))
  (loop for c in (paths p) append (path-resample c steps)))

(defmethod primitive-extents ((p text))
  (let ((w (width p)) (h (height p)) (d (depth p)))
    (list (vec-p 0 (- d))
          (vec-p w (- d))
          (vec-p w h)
          (vec-p 0 h))))

(defmethod primitive-centroid ((p text))
  (centroid-of-points (primitive-extents p)))

(defun text->rect (p)
  (let* ((w (width p)) (h (height p)) (d (depth p))
         (y (/ (- h d) 2d0)))
    (values (make-instance 'rect :w w :h (+ h d))
            (vec-4 (/ w 2d0) y 0 0))))

(defmethod primitive-anchor ((p text) key &rest args)
  (declare (ignore args))
  (multiple-value-bind (rect offset) (text->rect p)
    (case key
      (:base      (vec-p 0 0))
      (:base-east (vec-p (width p) 0))
      (:base-west (vec-p 0 0))
      (:mid       (vec-p (/ (width p) 2d0) 0))
      (t (v+ offset (primitive-anchor rect key))))))

(defmethod primitive-boundary ((p text) direction)
  (multiple-value-bind (rect offset) (text->rect p)
    (v+ offset (primitive-boundary rect direction))))


(defun render-package (package)
  (if (cdr package)
      (format nil "\\usepackage[~{~a~^,~}]{~a}" (rest package) (first package))
      (format nil "\\usepackage{~a}" (first package))))

(defun render-packages (packages)
  (format nil "~{~a~%~}" (mapcar #'render-package packages)))

(defun build-latex-preamble (&optional (class *latex-doc-class*) (packages *latex-packages*))
  (format nil "\\documentclass{~a}~%~a" class (render-packages packages)))

(defun build-document (text
                       &optional (class *latex-doc-class*)
                                 (packages *latex-packages*))
  (format nil "~a\\begin{document}~%\\begin{preview}~%~a~%\\end{preview}~%\\end{document}~%"
          (build-latex-preamble class packages)
          text))

(defparameter *tex-char-escapes*
  '((#\\ . "\\textbackslash{}")
    (#\{ . "\\{")
    (#\} . "\\}")
    (#\# . "\\#")
    (#\% . "\\%")
    (#\& . "\\&")
    (#\_ . "\\_")
    (#\^ . "\\textasciicircum{}")
    (#\~ . "\\textasciitilde{}")))

(define-condition tex-escape-error (error)
  ((text :initarg :text :reader tex-escape-error-text)
   (why :initarg :why :reader tex-escape-error-why))
  (:report (lambda (c s)
             (format s "~a: ~s"
                     (tex-escape-error-why c)
                     (tex-escape-error-text c)))))



(defun escape-tex (text)
  (with-output-to-string (out)
    (let ((math-mode nil)
          (i 0)
          (n (length text)))
      (loop while (< i n)
            for c = (char text i)
            do (cond
                 ((and (char= c #\\) (not math-mode) (< (1+ i) n)
                       (member (char text (1+ i)) '(#\$ #\\))) 
                  (write-string (if (char= (char text (1+ i)) #\$) "\\$" "\\textbackslash{}"))
                  (incf i 1))
                 ((char= c #\$)
                  (write-char c)
                  (setf math-mode (not math-mode)))
                 (math-mode (write-char c))
                 (t
                  (let ((esc (cdr (assoc c *tex-char-escapes*)))) ; All other chars are escaped normally
                    (if esc
                        (write-string esc)
                        (write-char c)))))
               (incf i))
      ;; Better not end up in math mode
      (when math-mode
        (error 'tex-escaping-error :text text :why "Mismatched $")))))

(defun latex-split-log (log)
  (loop for line in (uiop:split-string log :separator '(#\Newline))
        if (and (> (length line) 1)
                (char= (char line 0) #\!))
          if (search "! Preview:" line)
            if (search "ended.(" line) 
              collect (string-right-trim '(#\Return) line) into previews
        end
        else
          collect (string-right-trim '(#\Return) line) into errors
        end
        end
        finally (return (values previews errors))))


(esrap:defrule preview-metrics (and 
                                svg-digits #\+
                                svg-digits #\x
                                svg-digits)
  (:lambda (s)
    (list (parse-integer (nth 0 s))
          (parse-integer (nth 2 s))
          (parse-integer (nth 4 s)))))

(esrap:defrule preview-metrics-line (and (* (and (not preview-metrics) character))
                                         preview-metrics
                                         (* (and (not preview-metrics) character)))
  (:lambda (s)
    (second s)))





(defun parse-log (log-path)
  (let ((log (with-open-file (f log-path :direction :input)
               (uiop:read-file-string f))))
    (multiple-value-bind (preview errors) (latex-split-log log)
      (when errors
        (error "LaTeX failed:~%~{~a~^~%~}" errors))
      (unless preview
        (error "Did not find any previews"))
      (esrap:parse 'preview-metrics-line (first preview)))))


(defun math-rect-path (x y w h)
  (path-from-points (list (vec-p x (- y))
                          (vec-p (+ x w) (- y))
                          (vec-p (+ x w) (- (+ y h)))
                          (vec-p x (- (+ y h)))
                          (vec-p x (- y)))))




(defun math-parse-svg (svg)
  (let* ((root (xmls:parse svg))
         (scale (/ 1d0 *pt-to-world*))
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
                    (let ((id (subseq (or (xml-attr node "href")
                                          (xml-attr node "xlink:href")
                                          (error "<use> with no corresponding dvisvgm output"))
                                      1))
                          (x (xml-attr-num node "x"))
                          (y (xml-attr-num node "y")))
                      (dolist (p (gethash id outlines))
                        (push (call-path-points
                               (lambda (vec)
                                 (vec-p (+ (vec-x vec) x)
                                        (- (+ (vec-y vec) y))))
                               p)
                              paths))))
                   ((equal tag "rect")
                    (push (math-rect-path
                           (xml-attr-num node "x")
                           (xml-attr-num node "y")
                           (xml-attr-num node "width")
                           (xml-attr-num node "height"))
                          paths))
                   (t (mapc #'walk (xmls:node-children node)))))))
      (walk root))
    (mapcar (lambda (p)
                            (call-path-points
                             (lambda (v) (vec-p (* scale (vec-x v))
                                                (* scale (vec-y v))))
                             p))
                          (nreverse paths))))

(defun text->svg->text (text)
  (let* ((doc (build-document text))
         (name (hash-string doc))
         (tex-path (merge-pathnames (format nil "~d.tex" name) *math-cache-dir*))
         (dvi-path (merge-pathnames (format nil "~d.dvi" name) *math-cache-dir*))
         (svg-path (merge-pathnames (format nil "~d.svg" name) *math-cache-dir*))
         (log-path (merge-pathnames (format nil "~d.log" name) *math-cache-dir*)))
    (ensure-directories-exist tex-path)

    (unless (uiop:file-exists-p dvi-path)
      (with-open-file (s tex-path :direction :output :if-exists :supersede)
        (write-string doc s))
      (run-external *latex-program*
                    (list "-interaction=nonstopmode" tex-path)
                    :directory *math-cache-dir*
                    :ignore-error-status t))

    (unless (uiop:file-exists-p svg-path)
      (run-external *dvisvgm-program*
                    `("--no-fonts" "--exact-bbox" "-o" ,svg-path
                                   ,dvi-path)
                    :directory *math-cache-dir*))
    (destructuring-bind (height depth width) (parse-log log-path)
      (make-instance 'text
        :paths (math-parse-svg (with-open-file (s svg-path)
                                 (uiop:read-file-string s)))
        :text-str text
        :width (/ width +TEX-POINTS-PER-POINT+)
        :height (/ height +TEX-POINTS-PER-POINT+)
        :depth (/ depth +TEX-POINTS-PER-POINT+)))))



(defun text-align-offset (box align baseline)
  (let ((w (width box))
        (h (height box))
        (d (depth box)))
    (vec-4 (ecase align
             (:left 0d0)
             (:center (- (/ w 2d0)))
             (:right (- w)))
           (ecase baseline
             (:base 0d0)
             (:middle (- (/ (- h d) 2d0)))
             (:top (- h))
             (:bottom d))
           0d0 0d0)))

(defun text-size (&optional style)
  (to-df (or (getf (or style *style*) :font-size) *pt-to-world*)))

(defun measure-text (text &key size raw style)
  (let ((box (text-box (if raw text (escape-tex text))))
        (em (to-df (or size (text-size style)))))
    (values (* em (width box))
            (* em (height box))
            (* em (depth box)))))

(defun text-style (style)
  (merge-style style
               '(:fill "black" :stroke-width 0.001)))

(defun draw-text (text &key size (align :left) (baseline :base) name style at)
  (let* ((style (merge-style *style* style))
         (size (to-df (or size (text-size style))))
         (prim (text->svg->text text))
         (offset (text-align-offset prim align baseline)))
    (with-at at
      (with-transform (mm-* (mat-4-scale size size size)
                            (mat-4-translate (vec-x offset) (vec-y offset) 0d0))
        (emit prim
              :name name
              :style (text-style style))))))

