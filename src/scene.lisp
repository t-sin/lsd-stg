(defpackage #:lsd.scene
  (:use #:cl)
  (:export #:scene
           #:scene-width
           #:scene-height
           #:scene-window
           #:scene-resources
           #:draw
           #:update
           #:input
           #:load-resources))
(in-package #:lsd.scene)

(defclass scene ()
  ((width :initform 800
          :initarg :width
          :accessor scene-width)
   (height :initform 600
           :initarg :height
           :accessor scene-height)
   (resources :accessor scene-resources)))

(defgeneric load-resources (scene renderer))
(defgeneric draw (scene renderer))
(defgeneric update (scene))
(defgeneric input (scene &rest keys &key &allow-other-keys))
