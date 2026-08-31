(in-package :quickdraw)

(esrap:defrule svg-digit (esrap:character-ranges (#\0 #\9)))

(esrap:defrule svg-digits (+ svg-digit)
  (:text t))

(esrap:defrule svg-sign (or #\+ #\-)
  (:lambda (s) (if (string= s "-") -1 1)))

(esrap:defrule svg-whitespace (or #\Space #\Tab #\Newline #\Return #\Page))

(esrap:defrule svg-whitespace* (* svg-whitespace)
  (:constant nil))

(esrap:defrule svg-sep
    (or (and (+ svg-whitespace) (esrap:? #\,) svg-whitespace*)
        (and #\, svg-whitespace*))
  (:constant nil))

(esrap:defrule svg-sep? (esrap:? svg-sep) (:constant nil))

(esrap:defrule svg-num-frac (and #\. (esrap:? svg-digits))
  (:destructure (dot digits)
    (declare (ignore dot))
    (if digits
        (/ (parse-integer digits) (expt 10 (length digits)))
        0)))

(esrap:defrule svg-num-with-whole (and svg-digits (esrap:? svg-num-frac))
  (:destructure (whole fraction)
    (+ (parse-integer whole) (or fraction 0))))

(esrap:defrule svg-float-plain (or svg-num-with-whole svg-num-frac))

(esrap:defrule svg-exponent (and (or #\e #\E) (esrap:? svg-sign) svg-digits)
  (:destructure (e sign digits)
    (declare (ignore e))
    (* (or sign 1) (parse-integer digits))))

(esrap:defrule svg-number
    (and (esrap:? svg-sign) svg-float-plain (esrap:? svg-exponent))
  (:destructure (sign num exponent)
    (coerce (* (or sign 1) num (expt 10 (or exponent 0)))
            'double-float)))

(esrap:defrule svg-flag (or #\0 #\1)
  (:lambda (s) (string= s "1")))


;; (esrap:defrule svg-coordinate svg-number
;;   (:lambda (n) (list n)))

(esrap:defrule svg-coordinate (and svg-number svg-sep? svg-number)
  (:destructure (x s y) (declare (ignore s)) (list x y)))

(esrap:defrule svg-2-coords
    (and svg-coordinate svg-sep? svg-coordinate)
  (:destructure (a s b) (declare (ignore s)) (append a b)))

(esrap:defrule svg-3-coords
    (and svg-coordinate svg-sep? svg-coordinate svg-sep? svg-coordinate)
  (:destructure (a s1 b s2 c)
    (declare (ignore s1 s2))
    (append a b c)))

(esrap:defrule svg-arc-args
    (and svg-number svg-sep? svg-number svg-sep? svg-number
         svg-sep? svg-flag svg-sep? svg-flag svg-sep?
         svg-coordinate)
  (:destructure (major s1 minor s2 rotation s3 large s4 sweep s5 target)
    (declare (ignore s1 s2 s3 s4 s5))
    (list* major minor rotation large sweep target)))



(defmacro define-svg-seq (rule-name elem)
  `(esrap:defrule ,rule-name (and ,elem (* (and svg-sep? ,elem)))
     (:destructure (head tail)
       (cons head (mapcar #'second tail)))))

(define-svg-seq svg-num-seq svg-number)
(define-svg-seq svg-coord-seq    svg-coordinate)
(define-svg-seq svg-2-coord-seq  svg-2-coords)
(define-svg-seq svg-3-coord-seq  svg-3-coords)
(define-svg-seq svg-arc-seq      svg-arc-args)

(defmacro define-svg-command (name cmd op args)
  (let ((upper (char-upcase cmd))
        (lower (char-downcase cmd)))
    `(esrap:defrule ,name (and (or ,upper ,lower) svg-whitespace* ,args)
       (:destructure (letter whitespace args)
         (declare (ignore whitespace))
         (list ,op (string= letter ,(string lower)) args)))))

(define-svg-command svg-moveto         #\M :move         svg-coord-seq)
(define-svg-command svg-lineto         #\L :line         svg-coord-seq)
(define-svg-command svg-hlineto        #\H :hline        svg-num-seq)
(define-svg-command svg-vlineto        #\V :vline        svg-num-seq)
(define-svg-command svg-curveto        #\C :cubic        svg-3-coord-seq)
(define-svg-command svg-smooth-curveto #\S :smooth-cubic svg-2-coord-seq)
(define-svg-command svg-quadto         #\Q :quad         svg-2-coord-seq)
(define-svg-command svg-smooth-quadto  #\T :smooth-quad  svg-coord-seq)
(define-svg-command svg-arcto          #\A :arc          svg-arc-seq)





(esrap:defrule svg-close (or #\Z #\z)
  (:lambda (letter) (declare (ignore letter))
    (list :close nil nil)))

(esrap:defrule svg-drawto
    (or svg-close
        svg-lineto
        svg-hlineto
        svg-vlineto
        svg-curveto
        svg-smooth-curveto
        svg-quadto
        svg-smooth-quadto
        svg-arcto))

(esrap:defrule svg-drawto-seq
    (* (and svg-whitespace* svg-drawto))
  (:lambda (items) (mapcar #'second items)))

(esrap:defrule svg-move-draw (and svg-moveto svg-drawto-seq)
  (:destructure (move draws) (cons move draws)))



(esrap:defrule svg-path-data
    (and svg-whitespace* (* (and svg-move-draw svg-whitespace*)))
  (:destructure (leading groups)
    (declare (ignore leading))
    (loop for group in groups append (first group))))


(defun svg-vector-angle (ux uy vx vy)
  (let* ((d (+ (* ux vx) (* uy vy)))
         (len (* (sqrt (+ (* ux ux) (* uy uy)))
                 (sqrt (+ (* vx vx) (* vy vy)))))
         (cosine (if (< len +epsilon+)
                     0d0
                     (max -1d0 (min 1d0 (/ d len))))))
    (* (if (minusp (- (* ux vy) (* uy vx))) -1d0 1d0)
       (acos cosine))))

(defun svg-arc-segment (p0 p1 rx ry rotation large-arc sweep)
  (let ((rx (abs rx))
        (ry (abs ry)))
    (cond
      ((< (magnitude (v- p1 p0)) +epsilon+) nil)
      ((or (< rx +epsilon+) (< ry +epsilon+))
       (make-instance 'path-segment-line :start p0 :end p1))
      (t
       (let* ((phi (deg->rad rotation))
              (cs (cos phi))
              (sn (sin phi))
              (dx (/ (- (vec-x p0) (vec-x p1)) 2d0))
              (dy (/ (- (vec-y p0) (vec-y p1)) 2d0))
              (x1 (+ (* cs dx) (* sn dy)))
              (y1 (- (* cs dy) (* sn dx)))
              (oversize (+ (/ (* x1 x1) (* rx rx))
                           (/ (* y1 y1) (* ry ry))))
              (scaled (> oversize 1d0)))
         (when scaled
           (let ((s (sqrt oversize)))
             (setf rx (* rx s)
                   ry (* ry s))))
         (let* ((rx2 (* rx rx))
                (ry2 (* ry ry))
                (den (+ (* rx2 y1 y1) (* ry2 x1 x1)))
                (coef (if scaled
                          0d0
                          (* (if (eq large-arc sweep) -1d0 1d0)
                             (sqrt (/ (max 0d0 (- (* rx2 ry2) den)) den)))))
                (cx1 (* coef (/ (* rx y1) ry)))
                (cy1 (- (* coef (/ (* ry x1) rx))))
                (ux (/ (- x1 cx1) rx))
                (uy (/ (- y1 cy1) ry))
                (vx (/ (- (- x1) cx1) rx))
                (vy (/ (- (- y1) cy1) ry))
                (theta (svg-vector-angle 1d0 0d0 ux uy))
                (delta (svg-vector-angle ux uy vx vy)))
           (cond ((and (not sweep) (> delta 0d0)) (decf delta (* 2 pi)))
                 ((and sweep (< delta 0d0)) (incf delta (* 2 pi))))
           (make-arc-segment
            (vec-p (+ (- (* cs cx1) (* sn cy1))
                      (/ (+ (vec-x p0) (vec-x p1)) 2d0))
                   (+ (* sn cx1) (* cs cy1)
                      (/ (+ (vec-y p0) (vec-y p1)) 2d0)))
            (vec-dir (* rx cs) (* rx sn))
            (vec-dir (* (- ry) sn) (* ry cs))
            :start theta
            :sweep delta)))))))


(defmacro iter-bind-list (l vars &rest body)
  (let ((v (gensym)))
    `(dolist (,v ,l)
       (destructuring-bind ,vars ,v
         ,@body))))


(defun svg-commands->paths (commands)
  (let ((paths nil)
        (segments nil)
        (subpath-start (vec-p 0 0))
        (current (vec-p 0 0))
        (cubic-control nil)
        (quad-control nil))
    (labels
        ((finish-subpath (closed)
           (when segments
             (push (make-path (nreverse segments)
                              :closed (and closed
                                           (> (magnitude (v- current subpath-start))
                                              +epsilon+)))
                   paths))
           (setf segments nil))
         (point (relative x y)
           (if relative
               (vec-p (+ (vec-x current) x) (+ (vec-y current) y))
               (vec-p x y)))
         (reflect (control)
           (if control
               (v- (scale-vec 2d0 current) control)
               current))
         (line-to (target)
           (push (make-instance 'path-segment-line :start current :end target)
                 segments)
           (setf current target cubic-control nil quad-control nil))
         (cubic-to (c1 c2 target)
           (push (make-instance 'path-segment-bez3
                   :p0 current :p1 c1 :p2 c2 :p3 target)
                 segments)
           (setf current target cubic-control c2 quad-control nil))
         (quad-to (c target)
           (push (make-instance 'path-segment-bez2
                   :p0 current :p1 c :p2 target)
                 segments)
           (setf current target cubic-control nil quad-control c))
         (arc-to (rx ry rotation large sweep target)
           (let ((seg (svg-arc-segment current target rx ry rotation large sweep)))
             (when seg (push seg segments)))
           (setf current target cubic-control nil quad-control nil)))
      (dolist (command commands)
        (destructuring-bind (op relative tuples) command
          (ecase op
            (:move
             (finish-subpath nil)
             (loop for tuple in tuples
                   for firstp = t then nil
                   do (destructuring-bind (x y) tuple
                        (let ((target (point relative x y)))
                          (if firstp
                              (setf subpath-start target
                                    current target
                                    cubic-control nil
                                    quad-control nil)
                              (line-to target))))))
            (:line
             (dolist (tuple tuples)
               (destructuring-bind (x y) tuple
                 (line-to (point relative x y)))))
            (:hline
             (dolist (x tuples)
               (line-to (vec-p (if relative (+ (vec-x current) x) x)
                               (vec-y current)))))
            (:vline
             (dolist (y tuples)
               (line-to (vec-p (vec-x current)
                               (if relative (+ (vec-y current) y) y)))))
            (:cubic
             (iter-bind-list tuples (x1 y1 x2 y2 x y)
                             (cubic-to (point relative x1 y1)
                                       (point relative x2 y2)
                                       (point relative x y))))
            (:smooth-cubic
             (iter-bind-list tuples (x2 y2 x y)
                             (cubic-to (reflect cubic-control)
                                       (point relative x2 y2)
                                       (point relative x y))))
            (:quad
             (iter-bind-list tuples (x1 y1 x y)
                             (quad-to (point relative x1 y1)
                                      (point relative x y))))
            (:smooth-quad
             (iter-bind-list tuples (x y)
                             (quad-to (reflect quad-control)
                                      (point relative x y))))
            (:arc
             (iter-bind-list tuples (rx ry rotation large sweep x y)
                             (arc-to rx ry rotation large sweep (point relative x y))))
            (:close
             (finish-subpath t)
             (setf current subpath-start
                   cubic-control nil
                   quad-control nil)))))
      (finish-subpath nil)
      (nreverse paths))))


(defun parse-svg-num (data)
  (esrap:parse 'svg-number data))

(defun parse-svg-path-commands (data)
  (esrap:parse 'svg-path-data data))

(defun parse-svg-path (data)
  (svg-commands->paths (parse-svg-path-commands data)))
