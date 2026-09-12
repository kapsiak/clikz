(in-package :clikz)

(defvar *keep-temp-files* nil)
(defvar *external-program-error-lines* 40)


;; Make errors more readable if (when) they occur
(defun last-lines (text n)
  (let ((lines (uiop:split-string
                (string-right-trim '(#\Newline #\Return) text)
                :separator '(#\Newline))))
    (if (<= (length lines) n)
        text
        (format nil "[~d ...]~%~{~a~^~%~}"
                (- (length lines) n)
                (last lines n)))))



(define-condition external-program-not-found (error)
  ((program :initarg :program :reader external-program-name))
  (:report (lambda (c s)
             (format s "The program ~s was not found on PATH."
                     (external-program-name c)))))

;; Needed to diagnose problems with TeX compilation
(define-condition external-program-error (error)
  ((command :initarg :command :reader external-program-command)
   (exit-code    :initarg :exit-code  :initform nil   :reader external-program-exit-code)
   (output  :initarg :output  :initform "" :reader external-program-output)
   (errors  :initarg :errors  :initform "" :reader external-program-errors)
   (directory :initarg :directory :initform nil :reader external-program-directory))
  (:report
   (lambda (c s)
     (format s "~a~%exited with code ~a."
             (uiop:escape-command (external-program-command c))
             (external-program-exit-code c))
     (when (external-program-directory c)
       (format s "~%Working directory: ~a" (external-program-directory c)))
     (flet ((text-block (label text)
              (let ((text
                      (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
                (unless (zerop (length text))
                  (format s "~%~%~a:~%~a" label
                          (last-lines text *external-program-error-lines*))))))
       (text-block "stderr" (external-program-errors c))
       (text-block "stdout" (external-program-output c))))))


(defun find-program (program)
  "Return either absolute path if provided or a search through the PATH env var "
  (let ((name (string program)))
    (if (find #\/ name)
        (and (uiop:file-exists-p name)
             (pathname name))
        (loop for dir in (uiop:getenv-pathnames "PATH")
              for c = (merge-pathnames
                       name (uiop:ensure-directory-pathname dir))
              when (uiop:file-exists-p c) return c))))


(defun program-or-err (program)
  (or (find-program program)
      (error 'external-program-not-found :program (string program))))

(defun run-system (program args &key directory input ignore-error-status)
  "Run external program raising error if the exit code is nonzero. Optionally use input for standard input."
  (let* ((path (program-or-err program))
         (args (mapcar #'princ-to-string args))
         (directory (and directory (uiop:ensure-directory-pathname directory))))
    (flet ((run (in)
             (uiop:run-program (cons (uiop:native-namestring path) args)
                               :input in
                               :output :string
                               :error-output :string
                               :ignore-error-status t
                               :directory directory)))
      (multiple-value-bind (out err code)
          (if (stringp input)
              (with-input-from-string (in input) (run in))
              (run input))
        (when (and
               (not ignore-error-status)
               (not (eql code 0)))
          (error 'external-program-error
                 :command (cons (string program) args)
                 :exit-code code
                 :output out :errors err
                 :directory directory))
        (values out err code)))))


(defun make-temporary-directory (&optional (prefix "clikztmp"))
  (loop for i below 1024
        for name = (format nil "~a-~d-~d-~d"
                           prefix
                           (random 1000000)  
                           i) ; This sould be sufficiently unique for any practical use case.
        for dir = (uiop:ensure-directory-pathname
                   (merge-pathnames name (uiop:temporary-directory)))
        unless (uiop:directory-exists-p dir)
          do (ensure-directories-exist dir)
             (return dir)
        finally (error "Could not create a temporary directory under ~a."
                       (uiop:temporary-directory))))

(defmacro with-temporary-directory ((var &key (prefix "clikz")) &body body)
  "Create a temp dir, optionally removing when the form finishes"
  `(let ((,var (make-temporary-directory ,prefix)))
     (unwind-protect (progn ,@body)
       (cond (*keep-temp-files*
              (format *error-output* "~&Kept temporary directory ~a~%" ,var))
             ((uiop:subpathp ,var (uiop:temporary-directory))
              (uiop:delete-directory-tree ,var :validate t
                                               :if-does-not-exist :ignore))
             (t (warn "Refusing to delete ~a: not under ~a."
                      ,var (uiop:temporary-directory)))))))


