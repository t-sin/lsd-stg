(defpackage #:hoard
  (:use #:cl)
  (:export #:create-id
           #:resource
           #:resource-id
           #:resource-name
           #:resource-type
           #:resource-pathname
           #:resource-loaded-p
           #:resource-obj
           #:load-resource
           #:register
           #:register-by-name
           #:get-by-name))
(in-package #:hoard)

(defparameter *resource-count* 0)

(defun create-id ()
  (prog1 *resource-count*
    (incf *resource-count*)))

;; loaded-p or parameters for read streaming?
(defclass resource ()
  ((id :type integer
       :initform (create-id)
       :accessor resource-id)
   (name :type (or (member nil) keyword)
         :initarg :name
         :accessor resource-name)
   (type :type keyword
         :initarg :type
         :accessor resource-type)
   (pathname :type string
             :initarg :pathname
             :accessor resource-pathname)
   (loaded-p :type (member nil :streaming :loaded)
             :initarg nil
             :accessor resource-loaded-p)
   (obj :initarg :obj
        :accessor resource-obj)))

(defgeneric load-resource (res context))
(defmethod load-resource (res context)
  (declare (ignore context))
  (setf (resource-loaded-p res) t))

;;(defgeneric start-streaming (res))

(defparameter *resources-by-name* (make-hash-table :test #'eq))

(defgeneric register (type name pathname))

(defun register-by-name (res)
  (setf (gethash (resource-name res) *resources-by-name*) res)
  res)

(defun get-by-name (name)
  (gethash name *resources-by-name*))
