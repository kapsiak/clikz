(in-package :quickdraw)


(defclass resource ()
  ((id :initarg :id :accessor resource-id)))

(defgeneric resource-key (resource))

(defun intern-resource (resource)
  (let ((key (resource-key resource)))
    (or (gethash key *resources*)
        (setf (gethash key *resources*)
              (progn (setf (resource-id resource)
                           (format nil "RID~d" (incf *resource-counter*)))
                     resource)))))
