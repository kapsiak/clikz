(in-package :clikz)

(defun style-color (c)
  (typecase c
    (color c)
    (resource c)
    (null "none")
    ((or string symbol)
     (let ((s (string-downcase (string c))))
       (if (member s '("none" "currentcolor" "transparent") :test #'string=)
           s
           (or (ignore-errors (parse-color c)) c))))
    (t c)))

(defstyle :stroke (c) (list :stroke (style-color c)))
(defstyle :fill   (c) (list :fill   (style-color c)))
(defstyle :text-color (c) (list :text-color (style-color c)))
(defstyle :color (c) (list :stroke c))
(defstyle :lw (n) (list :stroke-width n))
(defstyle :dashed   () '(:stroke-dasharray "6 4"))
(defstyle :dotted   () '(:stroke-dasharray "2 4" :stroke-linecap "round"))
(defstyle :dash-dot () '(:stroke-dasharray "6 4 2 4"))
(defstyle :stroked ()  '(:fill "none"))
(defstyle :filled  ()  '(:stroke "none"))
