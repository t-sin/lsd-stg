(defpackage #:lsd.scene
  (:use #:cl)
  (:export #:scene
           #:scene-width
           #:scene-height
           #:draw
           #:update
           #:input))
(in-package #:lsd.scene)

(defclass scene ()
  ((width :initform 800
          :initarg :width
          :accessor scene-width)
   (height :initform 600
           :initarg :height
           :accessor scene-height)))

(defgeneric draw (scene renderer))
(defgeneric update (scene))
(defgeneric input (scene &rest keys &key &allow-other-keys))
