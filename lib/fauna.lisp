(defpackage #:fauna
  (:use #:cl)
  (:import-from #:alexandria
                #:once-only)
  (:import-from #:anaphora
                #:aif
                #:it)
  (:export #:make-env*
           #:with-init
           #:with-window
           #:run))
(in-package #:fauna)

(defconstant +opengl-major-version+ 4)
(defconstant +opengl-minor-version+ 3)

(defstruct env
  title
  width
  height
  init-fn
  term-fn
  loop-fn
  event-fn)

(glfw:def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (glfw:set-window-should-close)))

(defun default-init-fn (env context)
  (declare (ignorable env context)))

(defun default-term-fn (env context)
  (declare (ignorable env context)))

(defun default-loop-fn (env context)
  (declare (ignorable env context)))

(defun default-event-fn (env context)
  (declare (ignorable env context)))

(defun make-env* (title width height &key init-fn term-fn loop-fn event-fn)
  (make-env :title title
            :width width
            :height height
            :init-fn (if (null init-fn)
                         #'default-init-fn
                         init-fn)
            :term-fn (if (null term-fn)
                         #'default-term-fn
                         term-fn)
            :loop-fn (if (null loop-fn)
                         #'default-loop-fn
                         loop-fn)
            :loop-fn (if (null loop-fn)
                         #'default-event-fn
                         event-fn)))

(defmacro with-init ((env context) &body body)
  (once-only (env context)
    `(unwind-protect
          (progn
            (glfw:initialize)
            (funcall (env-init-fn ,env) ,env ,context)
            ,@body)
       (progn
         (funcall (env-term-fn ,env) ,env ,context)
         (glfw:terminate)))))

(defmacro letw ((var env &rest keys &key &allow-other-keys) &body body)
  (flet ((getkey (name)
           (let ((name (symbol-name name)))
             `(aif (and (not (null ',keys)) (getf ',keys ,(intern name :keyword)))
                   it
                   (,(intern (format nil "ENV-~a" name)) ,env)))))
      `(let ((,var (progn
                     (glfw:create-window :title ,(getkey :title)
                                         :width ,(getkey :width)
                                         :height ,(getkey :height))
                     glfw:*window*)))
         ,@body)))

(defmacro with-window ((var env &rest keys &key &allow-other-keys) &body body)
  `(letw (,var ,env ,@keys)
     (unwind-protect
          (progn
            (glfw:set-key-callback 'quit-on-escape)
            ,@body)
       (glfw:destroy-window ,var))))

(defun run (window env context)
  (loop
    :until (glfw:window-should-close-p window)
    :do (funcall (env-loop-fn env) env context)
    :do (glfw:poll-events)))
