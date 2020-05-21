(defpackage #:lsd.shooter.component
  (:use #:cl
        #:lsd.shooter.scene)
  (:export #:actor  ; actors
           #:actor-id
           #:actor-type
           #:actor-tick
           #:actor-x
           #:actor-y
           #:actor-px
           #:actor-py
           #:actor-vx
           #:actor-vy
           #:actor-rad
           #:actor-used
           #:actor-code
           #:actor-tcode
           #:actor-pstack
           #:actor-gstack
           #:hitable  ; hitables
           #:hitable-id
           #:hitable-radius
           #:input  ; user inputs
           #:input-id
           #:input-u
           #:input-d
           #:input-l
           #:input-r
           #:input-s
           #:input-z
           #:animated  ; animated objects
           #:animated-id
           #:animated-name
           #:animated-idx  ; sprite index
           #:animated-num  ; number of sprites
           #:animated-frame  ; current frame
           #:animated-wait  ; frames to flip next sprite
           #:animated-x  ; logical centor of x
           #:animated-y  ; logical centor of y
           #:animated-w  ; width of one sprite
           #:animated-h  ; height of one sprite
           ;; utilities
           #:actor-vanish
           #:make-entity-id
           #:make-entity
           #:get-component))
(in-package #:lsd.shooter.component)

(defparameter *entity-count* 0)
(defun make-entity-id ()
  (prog1 *entity-count*
    (incf *entity-count*)))

(defstruct actor
  id type tick
  x y
  px py
  vx vy
  rad
  used
  code tcode  pstack gstack)

(defstruct hitable id radius)

(defstruct input id u d l r s z)
(defstruct animated
  id
  idx num
  frame wait
  name
  x y w h)

(defun make-entity (db type &rest keys &key &allow-other-keys)
  (let* ((maker-name (intern (format nil "MAKE-~a" (symbol-name type)) :lsd.shooter.component))
         (maker (symbol-function maker-name))
         (id-name (intern (format nil "~a-ID" (symbol-name type)) :lsd.shooter.component))
         (getter-id (symbol-function id-name))
         (entity (apply maker keys))
         (type (intern (symbol-name (type-of entity)) :keyword)))
    (let ((table (gethash type db)))
      (when (null table)
        (let ((new-table (make-hash-table)))
          (setf (gethash type db) new-table
                table new-table)))
      (setf (gethash (funcall getter-id entity) table) entity))
    entity))

(defun get-component (shooter entity ctype)
  (let* ((db (shooter-db shooter))
         (components (gethash ctype db)))
    (unless (null components)
      (gethash entity components))))
