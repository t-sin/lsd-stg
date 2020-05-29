(defpackage #:pollen
  (:use #:cl)
  (:export))
(in-package #:pollen)

(defstruct point
  x y z
  (used nil))

(defstruct (emitter (:include point))
  (type :straight :type (member :straight :reverse))
  (tick 0 :type integer)
  (interval 1)
  (allocnum 150)
  )

(defstruct (particle (:include point))
  lt          ; lifetime
  th vth ath  ; direction in radians
  vx vy vz
  ax ay az)

(defstruct (system (:constructor make-system*))
  emitters
  particles)
  
(defun make-system (number-of-emitters number-of-particles)
  (let ((ems (make-array number-of-emitters
                         :element-type 'emitter))
        (pts (make-array number-of-particles
                         :element-type 'particle)))
    (loop
      :for i :from 0 :below number-of-emitters
      :do (setf (aref ems i) (make-emitter :x 0 :y 0 :z 0)))
    (Loop
      :For I :from 0 :below number-of-particles
      :do (setf (aref pts i) (make-particle :lt 0
                                            :x 0 :y 0 :z 0
                                            :th 0 :vth 0 :ath 0
                                            :vx 0 :vy 0 :vz 0
                                            :ax 0 :ay 0 :az 0)))
    (make-system* :emitters ems
                  :particles pts)))

(defun alloc-emitter (system x y z type)
  (let ((em (find nil (system-emitters system) :key #'point-used :test #'eq)))
    (when em
      (setf (point-x em) x
            (point-y em) y
            (point-z em) z
            (point-used em) t
            (emitter-type em) type)
      em)))

(defun alloc-particle (system lifetime th pos vel acc)
  (let ((p (find nil (system-particles system) :key #'point-used :test #'eq)))
    (when p
      (setf (point-used p) t
            (particle-lt p) lifetime
            (point-x p) (elt pos 0)
            (point-y p) (elt pos 1)
            (point-z p) (elt pos 2)
            (particle-th p) (elt th 0)
            (particle-vth p) (elt th 1)
            (particle-ath p) (elt th 2)
            (particle-vx p) (elt vel 0)
            (particle-vy p) (elt vel 1)
            (particle-vz p) (elt vel 2)
            (particle-ax p) (elt acc 0)
            (particle-ay p) (elt acc 1)
            (particle-az p) (elt acc 2))
      p)))

(defun update-emitter (e system)
  (when (zerop (mod (emitter-tick e) (emitter-interval e)))
    (loop
      :for n :from 0 :below (emitter-allocnum e)
      :do (alloc-particle system
                          (- (random 20) 5)
                          (vector 0 0 0)
                          (vector (point-x e) (point-y e) (point-z e))
                          (vector (- (random 10) 5)
                                  (- (random 10) 5)
                                  (- (random 10) 5))
                          (vector 0 0 0)))
    (incf (emitter-tick e))))

(defun update-particle (p)
  (incf (point-x p) (particle-vx p))
  (incf (point-y p) (particle-vy p))
  (incf (point-z p) (particle-vz p))
  (incf (particle-th p) (particle-vth p))
  (incf (particle-vth p) (particle-ath p))
  (incf (particle-vx p) (particle-ax p))
  (incf (particle-vy p) (particle-ay p))
  (incf (particle-vz p) (particle-az p))
  (incf (particle-lt p) -1)
  (when (minusp (particle-lt p))
    (setf (point-used p) nil)))

(defun update-system (system)
  (let ((ems (system-emitters system))
        (ps (system-particles system)))
    (sort ps (lambda (a b)
               (declare (ignore b))
               (when a t))
          :key #'point-used)
    (loop
      :for e :across ems
      :do (when (point-used e)
            (update-emitter e system)))
    (loop
      :for p :across ps
      :while (point-used p)
      :do (update-particle p))))
