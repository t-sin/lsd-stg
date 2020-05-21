(defpackage #:lsd.shooter.scene
  (:use #:cl
        #:lsd.scene)
  (:export #:shooter
           #:shooter-tick
           #:shooter-tick-enable-p
           #:shooter-events
           #:shooter-ep
           #:shooter-db
           #:shooter-actors
           #:shooter-hitables
           #:shooter-inputs
           #:shooter-animated
           #:shooter-screen-x
           #:shooter-screen-y
           #:shooter-screen-w
           #:shooter-screen-h))
(in-package #:lsd.shooter.scene)

(defclass shooter (scene)
  ((tick :initform 0
         :accessor shooter-tick)
   (tick-enable-p :initform t
                  :accessor shooter-tick-enable-p)
   (event-table :initarg :events
                :accessor shooter-events)
   (event-pointer :initform 0
                  :accessor shooter-ep)
   (database :initarg :db
             :accessor shooter-db)
   (actors :initarg :actors
           :accessor shooter-actors)
   (hitables :initarg :hitables
             :accessor shooter-hitables)
   (inputs :initarg :inputs
           :accessor shooter-inputs)
   (animated :initarg :animated
             :accessor shooter-animated)
   (screen-x :initform 200
         :initarg :sx
         :accessor shooter-screen-x)
   (screen-y :initform 25
         :initarg :sy
         :accessor shooter-screen-y)
   (screen-w :initform 400
             :initarg :sw
             :accessor shooter-screen-w)
   (screen-h :initform 550
             :initarg :sw
             :accessor shooter-screen-h)))
