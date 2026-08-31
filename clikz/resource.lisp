(in-package :quickdraw)

(defvar *resources* (make-hash-table :test #'equal))
(defvar *resource-index* nil)


(defclass resource ()
  ((id :initarg :id :accessor resource-id)))

(defgeneric resource-key (resource))


(defun intern-resource (resource)
  (or (gethash resource *resources*)
      (setf (gethash resource *resources*)
            (progn (setf (resource-id resource)
                         (format nil "RID~d" (incf *resource-index*)))
                   resource))))

(defclass clip-resource (resource)
  ((elements :initarg :elements :accessor clip-elements)))

(defclass linear-gradient-resource (resource)
  ((stops :initarg :stops :accessor gradient-stops)
   (direction :initarg :direction :accessor gradient-direction
              :initform (vec-2 1 0))))
