(defpackage #:lsd.shooter-scene
  (:use #:cl
        #:lsd.scene
        #:lsd.shooter.util
        #:lsd.shooter.scene
        #:lsd.shooter.component
        #:lsd.shooter.vm)
  (:export #:make-shooter))
(in-package #:lsd.shooter-scene)

(defstruct image
  w h texture)

(defun update-inputs (shooter &key (u nil u?) (d nil d?) (l nil l?) (r nil r?) (s nil s?) (z nil z?))
  (loop
    :for i :across (shooter-inputs shooter)
    :do (progn
          (when u? (setf (input-u i) u))
          (when d? (setf (input-d i) d))
          (when l? (setf (input-l i) l))
          (when r? (setf (input-r i) r))
          (when s? (setf (input-s i) s))
          (when z? (setf (input-z i) z)))))

(defun move-player (shooter)
  (let* ((p (find :player (shooter-actors shooter) :key #'actor-type))
         (i (get-component shooter (actor-id p) :input)))
    (let ((move (if (input-s i) 3 5)))
      (when (input-u i) (incf (actor-y p) (- move)))
      (when (input-d i) (incf (actor-y p) move))
      (when (input-l i) (incf (actor-x p) (- move)))
      (when (input-r i) (incf (actor-x p) move)))))

(defun move-actors (shooter)
  (loop
    :for a :across (shooter-actors shooter)
    :for x := (actor-x a)
    :for y := (actor-y a)
    :do (when (actor-used a)
          (setf (actor-x a) (+ x (actor-vx a))
                (actor-y a) (+ y (actor-vy a))))))

(defun check-collision (shooter)
  (let* ((player (find :player (shooter-actors shooter) :key #'actor-type))
         (ph (get-component shooter (actor-id player) :hitable))
         (pid (actor-id player))
         (px (actor-x player))
         (py (actor-y player))
         (particle-code (let ((*package* (find-package :lsd.shooter.vm)))
                          (read-from-string "(atick 18 gte (vanish) () if)"))))
    (loop
      :for h :across (shooter-hitables shooter)
      :for a := (get-component shooter (hitable-id h) :actor)
      :for x := (actor-x a)
      :for y := (actor-y a)
      :when (and (actor-used a)
                 (not (= (actor-id a) pid)))
      :do (when (and (not (null h))
                     (> x (- px 50))
                     (< x (+ px 50))
                     (> y (- py 50))
                     (< y (+ py 50)))
            (when (< (distance px py x y)
                     (+ (hitable-radius ph) (hitable-radius h)))
              (vanish-actor a shooter)
              (put-actor shooter x y
                         (if (zerop (actor-vx a))
                             (- x (actor-px a))
                             (actor-vx a))
                         (if (zerop (actor-vy a))
                             (- y (actor-py a))
                             (actor-vy a))
                         () () particle-code ()
                         :particle))))))

(defun draw-animated (shooter renderer)
  (sdl2:set-render-draw-color renderer 255 255 255 255)
  (let* ((db (scene-resources shooter)))
    (loop
      :for actor :across (shooter-actors shooter)
      :for anim := (get-component shooter (actor-id actor) :animated)
      :for img := (gethash (animated-name anim) db)
      :for img-w := (image-w img)
      :for img-h := (image-h img)
      :for anim-w := (animated-w anim)
      :for anim-h := (animated-h anim)
      :for offset-x := (* (animated-idx anim) anim-w)
      :with x0 := (shooter-screen-x shooter)
      :with y0 := (shooter-screen-y shooter)
      :when (actor-used actor)
      :do (sdl2:render-copy renderer (image-texture img)
                            :source-rect (sdl2:make-rect offset-x 0 anim-w anim-h)
                            :dest-rect (sdl2:make-rect (+ x0 (- (floor (actor-x actor)) (animated-x anim)))
                                                       (+ y0 (- (floor (actor-y actor)) (animated-y anim)))
                                                       anim-w anim-h))
      :do (when (zerop (mod (animated-frame anim) (animated-wait anim)))
            (setf (animated-idx anim)
                  (mod (1+ (animated-idx anim)) (animated-num anim))))
      :do (incf (animated-frame anim)))))

(defun process-events (shooter)
  (when (< (shooter-ep shooter)
           (length (shooter-events shooter)))
    (loop
      :for ep := (shooter-ep shooter) :then (incf ep)
      :while (< ep (length (shooter-events shooter)))
      :finally (setf (shooter-ep shooter) ep)
      :do (let ((e (aref (shooter-events shooter) ep)))
            (if (<= (car e) (shooter-tick shooter))
                (funcall (cdr e) shooter)
                (progn
                  (setf (shooter-ep shooter) ep)
                  (return)))))))

(defun update-ticks (shooter)
  (loop
    :for a :across (shooter-actors shooter)
    :when (actor-used a)
    :do (incf (actor-tick a))))

(defun eval-objects (shooter)
  (loop
    :for a :across (shooter-actors shooter)
    :for input := (get-component shooter (actor-id a) :input)
    :do (eval-object shooter a)))

(defmethod draw ((scene shooter) renderer)
  (sdl2:set-render-draw-color renderer 40 40 40 255)
  (sdl2:render-fill-rect renderer
                         (sdl2:make-rect 200 25 400 550))
  (draw-animated scene renderer)
  (let ((bg-1 (gethash :bg-1 (scene-resources scene))))
    (sdl2:render-copy renderer (image-texture bg-1)
                      :dest-rect (sdl2:make-rect 0 0 (image-w bg-1) (image-h bg-1))))
  (let ((bg-2 (gethash :bg-2 (scene-resources scene))))
    (sdl2:render-copy renderer (image-texture bg-2)
                      :dest-rect (sdl2:make-rect (shooter-screen-x scene) 0
                                                 (image-w bg-2) 25)))
  (let ((bg-3 (gethash :bg-3 (scene-resources scene))))
    (sdl2:render-copy renderer (image-texture bg-3)
                      :dest-rect (sdl2:make-rect (shooter-screen-x scene)
                                                 (+ 25 (shooter-screen-h scene))
                                                 (image-w bg-3) 25)))
  (let ((bg-4 (gethash :bg-4 (scene-resources scene))))
    (sdl2:render-copy renderer (image-texture bg-4)
                      :dest-rect (sdl2:make-rect (+ (shooter-screen-x scene) (shooter-screen-w scene))
                                                 0
                                                 (image-w bg-4) (image-h bg-4)))))

(defmethod update ((scene shooter))
  (process-events scene)
  (move-actors scene)
  (move-player scene)
  (check-collision scene)
  (eval-objects scene)
  (update-ticks scene)
  (when (shooter-tick-enable-p scene)
    (incf (shooter-tick scene))))

(defmethod handle-input ((scene shooter) &rest keys &key &allow-other-keys)
  (apply #'update-inputs `(,scene ,@keys)))

(defmethod load-resources ((scene shooter) renderer)
  (let ((db (make-hash-table)))
    (setf (scene-resources scene) db)
    (sdl2-image:init '(:png))
    (flet ((register (name path)
             (let* ((pathname (asdf:system-relative-pathname :lsd path))
                    (surface (sdl2-image:load-image pathname))
                    (texture (sdl2:create-texture-from-surface renderer surface))
                    (w (sdl2:surface-width surface))
                    (h (sdl2:surface-height surface)))
               (setf (gethash name db) (make-image :w w :h h :texture texture)))))
      (register :bg-1 "assets/bg_1.png")
      (register :bg-2 "assets/bg_2.png")
      (register :bg-3 "assets/bg_3.png")
      (register :bg-4 "assets/bg_4.png")
      (register :bullet0 "assets/bullet0.png")
      (register :bullet0-broken "assets/broken-bullet0.png")
      (register :bullet0-1 "assets/bullet0-1.png")
      (register :bullet0-2 "assets/bullet0-2.png")
      (register :particle0 "assets/particle0.png")
      (register :tapir "assets/lsd-tapir.png"))))

(defun load-script (pathname)
  (let ((pathname (asdf:system-relative-pathname :lsd pathname)))
    (with-open-file (in pathname
                        :direction :input
                        :element-type 'character)
      (loop
        :for sexp := (let ((*package* (find-package :lsd.shooter.vm)))
                       (read in nil :eof))
        :with result := nil
        :until (eq sexp :eof)
        :finally (return-from load-script (first result))
        :do (push (eval sexp) result)))))

(defun make-shooter ()
  (let ((actors ())
        (inputs ())
        (hitables ())
        (animated ())
        (db (make-hash-table)))
    (flet ((make-player ()
             (let ((id (make-entity-id)))
               (push (make-entity db :actor
                                  :id id :type :player :tick 0 :used nil
                                  :x 200 :y 500 :px 0 :py 0 :vx 0 :vy 0
                                  :rad 0
                                  :code () :pstack () :gstack ())
                     actors)
               (push (make-entity db :hitable :id id :radius 6) hitables)
               (push (make-entity db :input :id id :u nil :d nil :l nil :r nil :z nil) inputs)
               (push (make-entity db :animated :id id
                                  :idx 0 :num 4
                                  :frame 0 :wait 10
                                  :name :tapir
                                  :x 20 :y 40 :w 40 :h 60)
                     animated)))
           (make-enemy ()
             (let ((id (make-entity-id)))
               (push (make-entity db :actor
                                  :id id :type :enemy :tick 0 :used nil
                                  :x 200 :y 100 :px 0 :py 0 :vx 0 :vy 0
                                  :rad 0
                                  :code () :pstack () :gstack ())
                     actors)
               (push (make-entity db :hitable :id id :radius 6) hitables)
               (push (make-entity db :animated :id id
                                  :idx 0 :num 1
                                  :frame 0 :wait 5
                                  :name :bullet0
                                  :x 8 :y 8 :w 16 :h 16)
                     animated)))
           (make-bullet ()
             (let ((id (make-entity-id)))
               (push (make-entity db :actor
                                  :id id :type :bullet :tick 0 :used nil
                                  :x 0 :y 0 :px 0 :py 0 :vx 0 :vy 0
                                  :rad 0
                                  :code () :pstack () :gstack ())
                     actors)
               (push (make-entity db :hitable :id id :radius 3) hitables)
               (push (make-entity db :animated :id id
                                  :idx 0 :num 1
                                  :frame 0 :wait 5
                                  :name :bullet0
                                  :x 8 :y 8 :w 16 :h 16)
                     animated)))
           (make-particle ()
             (let ((id (make-entity-id)))
               (push (make-entity db :actor
                                  :id id :type :particle :tick 0 :used nil
                                  :x 0 :y 0 :px 0 :py 0 :vx 0 :vy 0
                                  :rad 0
                                  :code () :pstack () :gstack ())
                     actors)
               (push (make-entity db :animated :id id
                                  :idx 0 :num 7
                                  :frame 0 :wait 3
                                  :name :bullet0-broken
                                  :x 8 :y 8 :w 16 :h 16)
                     animated))))
      (make-player)
      (loop :for _ :from 0 :upto 100 :do (make-enemy))
      (loop :for _ :from 0 :upto 2000 :do (make-bullet))
      (loop :for _ :from 0 :upto 500 :do (make-particle))
      (make-instance 'shooter
                     :db db
                     :events (load-script "scripts/stage-01.lisp")
                     :actors (coerce (nreverse actors) 'vector)
                     :inputs (coerce (nreverse inputs) 'vector)
                     :hitables (coerce (nreverse hitables) 'vector)
                     :animated (coerce (nreverse animated) 'vector)))))
