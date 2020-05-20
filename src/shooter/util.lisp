(defpackage #:lsd.shooter.util
  (:use #:cl)
  (:export #:distance))
(in-package #:lsd.shooter.util)

(defun distance (ax ay bx by)
  (sqrt (+ (expt (- bx ax) 2)
           (expt (- by ay) 2))))
