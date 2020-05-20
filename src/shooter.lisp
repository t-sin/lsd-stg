(defpackage #:lsd.shooter
  (:use #:cl
        #:lsd.scene)
  (:export #:shooter
           #:make-shooter))
(in-package #:lsd.shooter)

(defclass shooter (scene)
  ((tick :initform 0
         :accessor shooter-tick)
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
  code pstack gstack)

(defun actor-vanish (actor)
  (setf (actor-used actor) nil
        (actor-tick actor) 0))

(defstruct hitable id radius)

(defstruct input id u d l r s z)
(defstruct animated
  id
  idx num
  frame framenum
  name
  x y w h)

(defun make-entity (db type &rest keys &key &allow-other-keys)
  (let* ((maker-name (intern (format nil "MAKE-~a" (symbol-name type)) :lsd.shooter))
         (maker (symbol-function maker-name))
         (id-name (intern (format nil "~a-ID" (symbol-name type)) :lsd.shooter))
         (getter-id (symbol-function id-name))
         (entity (apply maker keys))
         (type (type-of entity)))
    (let ((table (gethash type db)))
      (when (null table)
        (let ((new-table (make-hash-table)))
          (setf (gethash type db) new-table
                table new-table)))
      (setf (gethash (funcall getter-id entity) table) entity))
    entity))

(defun get-component (shooter entity ctype)
  (let* ((db (shooter-db shooter))
         (components (gethash (intern (symbol-name ctype) :lsd.shooter) db)))
    (unless (null components)
      (gethash entity components))))

(defstruct machine
  code ip cstack)

(defun vm/>rad (actor machine shooter)
  (push (/ (* PI (pop (actor-pstack actor))) 180)
        (actor-pstack actor))
  (incf (machine-ip machine)))

(defun vm/>deg (actor machine shooter)
  (push (/ (* 180 (pop (actor-pstack actor))) PI)
        (actor-pstack actor))
  (incf (machine-ip machine)))

(defun vm/sin (actor machine shooter)
  (push (sin (pop (actor-pstack actor)))
        (actor-pstack actor))
  (incf (machine-ip machine)))

(defun vm/cos (actor machine shooter)
  (push (cos (pop (actor-pstack actor)))
        (actor-pstack actor))
  (incf (machine-ip machine)))

(defun vm/eq (actor machine shooter)
  (push (equal (pop (actor-pstack actor))
               (pop (actor-pstack actor)))
        (actor-pstack actor))
  (incf (machine-ip machine)))

(defun vm/or (actor machine shooter)
  (let ((b (pop (actor-pstack actor)))
        (a (pop (actor-pstack actor))))
    (push (or a b) (actor-pstack actor))
    (incf (machine-ip machine))))

(defun vm/and (actor machine shooter)
  (let ((b (pop (actor-pstack actor)))
        (a (pop (actor-pstack actor))))
    (push (and a b) (actor-pstack actor))
    (incf (machine-ip machine))))

(defun vm/gt (actor machine shooter)
  (let ((b (pop (actor-pstack actor)))
        (a (pop (actor-pstack actor))))
    (push (> a b) (actor-pstack actor))
    (incf (machine-ip machine))))

(defun vm/gte (actor machine shooter)
  (let ((b (pop (actor-pstack actor)))
        (a (pop (actor-pstack actor))))
    (push (>= a b) (actor-pstack actor))
    (incf (machine-ip machine))))

(defun vm/lt (actor machine shooter)
  (let ((b (pop (actor-pstack actor)))
        (a (pop (actor-pstack actor))))
    (push (< a b) (actor-pstack actor))
    (incf (machine-ip machine))))

(defun vm/lte (actor machine shooter)
  (let ((b (pop (actor-pstack actor)))
        (a (pop (actor-pstack actor))))
    (push (<= a b) (actor-pstack actor))
    (incf (machine-ip machine))))

(defun vm/mod (actor machine shooter)
  (push (let ((div (pop (actor-pstack actor))))
          (mod (pop (actor-pstack actor)) div))
        (actor-pstack actor))
  (incf (machine-ip machine)))

(defun vm/add (actor machine shooter)
  (push (+ (pop (actor-pstack actor))
           (pop (actor-pstack actor)))
        (actor-pstack actor))
  (incf (machine-ip machine)))

(defun vm/sub (actor machine shooter)
  (let ((b (pop (actor-pstack actor)))
        (a (pop (actor-pstack actor))))
    (push (- a b) (actor-pstack actor))
    (incf (machine-ip machine))))

(defun vm/mul (actor machine shooter)
  (push (* (pop (actor-pstack actor))
           (pop (actor-pstack actor)))
        (actor-pstack actor))
  (incf (machine-ip machine)))

(defun vm/div (actor machine shooter)
  (let ((b (pop (actor-pstack actor)))
        (a (pop (actor-pstack actor))))
    (push (/ a b) (actor-pstack actor))
    (incf (machine-ip machine))))

(defun vm/rnd (actor machine shooter)
  (push (random 1.0)
        (actor-pstack actor))
  (incf (machine-ip machine)))

(defun vm/cons (actor machine shooter)
  (let ((b (pop (actor-pstack actor)))
        (a (pop (actor-pstack actor))))
    (push (cons a b) (actor-pstack actor))
    (incf (machine-ip machine))))

(defun vm/car (actor machine shooter)
  (push (car (pop (actor-pstack actor)))
        (actor-pstack actor))
  (incf (machine-ip machine)))

(defun vm/cdr (actor machine shooter)
  (push (cdr (pop (actor-pstack actor)))
        (actor-pstack actor))
  (incf (machine-ip machine)))

(defun vm/v/rot (actor machine shooter)
  (let ((theta (pop (actor-pstack actor)))
        (y (pop (actor-pstack actor)))
        (x (pop (actor-pstack actor))))
    (push (- (* x (cos theta)) (* y (sin theta)))
          (actor-pstack actor))
    (push (+ (* x (sin theta)) (* y (cos theta)))
          (actor-pstack actor))
    (incf (machine-ip machine))))

(defun vm/v/mul (actor machine shooter)
  (let ((a (pop (actor-pstack actor)))
        (y (pop (actor-pstack actor)))
        (x (pop (actor-pstack actor))))
    (push (* a x) (actor-pstack actor))
    (push (* a y) (actor-pstack actor))
    (incf (machine-ip machine))))

(defun vm/if (actor machine shooter)
  (let* ((false-clause (pop (actor-pstack actor)))
         (true-clause (pop (actor-pstack actor)))
         (value (pop (actor-pstack actor))))
    (push (list :if
                (machine-code machine)
                (1+ (machine-ip machine)))
          (machine-cstack machine))
    (if value
        (setf (machine-code machine) true-clause
              (machine-ip machine) 0)
        (setf (machine-code machine) false-clause
              (machine-ip machine) 0))))

(defun vm/do (actor machine shooter)
  (let ((diff (pop (actor-pstack actor)))
        (e (pop (actor-pstack actor)))
        (s (pop (actor-pstack actor)))
        (proc (pop (actor-pstack actor))))
    (push (list :do
                (list s e diff)
                (machine-code machine)
                (1+ (machine-ip machine)))
          (machine-cstack machine))
    (push s (actor-pstack actor))
    (setf (machine-code machine) proc
          (machine-ip machine) 0)))

(defun vm/dup (actor machine shooter)
  (let ((a (pop (actor-pstack actor))))
    (push a (actor-pstack actor))
    (push a (actor-pstack actor))
    (incf (machine-ip machine))))

(defun vm/drop (actor machine shooter)
  (pop (actor-pstack actor))
  (incf (machine-ip machine)))

(defun vm/swap (actor machine shooter)
  (let ((a (pop (actor-pstack actor)))
        (b (pop (actor-pstack actor))))
    (push a (actor-pstack actor))
    (push b (actor-pstack actor))
    (incf (machine-ip machine))))

(defun vm/over (actor machine shooter)
  (let ((b (pop (actor-pstack actor)))
        (a (pop (actor-pstack actor))))
    (push a (actor-pstack actor))
    (push b (actor-pstack actor))
    (push a (actor-pstack actor))
    (incf (machine-ip machine))))

(defun vm/rot (actor machine shooter)
  (let ((c (pop (actor-pstack actor)))
        (b (pop (actor-pstack actor)))
        (a (pop (actor-pstack actor))))
    (push b (actor-pstack actor))
    (push c (actor-pstack actor))
    (push a (actor-pstack actor))
    (incf (machine-ip machine))))

(defun vm/<g (actor machine shooter)
  (push (pop (actor-gstack actor))
        (actor-pstack actor))
  (incf (machine-ip machine)))

(defun vm/<<g (actor machine shooter)
  (let ((a (pop (actor-gstack actor))))
    (push a (actor-gstack actor))
    (push a (actor-pstack actor))
    (incf (machine-ip machine))))

(defun vm/>g (actor machine shooter)
  (push (pop (actor-pstack actor))
        (actor-gstack actor))
  (incf (machine-ip machine)))

(defun vm/gtick (actor machine shooter)
  (push (shooter-tick shooter) (actor-pstack actor))
  (incf (machine-ip machine)))

(defun vm/atick (actor machine shooter)
  (push (actor-tick actor) (actor-pstack actor))
  (incf (machine-ip machine)))

(defun vm/getp (actor machine shooter)
  (push (actor-x actor) (actor-pstack actor))
  (push (actor-y actor) (actor-pstack actor))
  (incf (machine-ip machine)))

(defun vm/setp (actor machine shooter)
  (let ((y (pop (actor-pstack actor)))
        (x (pop (actor-pstack actor))))
    (setf (actor-px actor) (actor-x actor)
          (actor-py actor) (actor-y actor))
    (setf (actor-x actor) x
          (actor-y actor) y)
    (incf (machine-ip machine))))

(defun vm/getv (actor machine shooter)
  (push (actor-vx actor) (actor-pstack actor))
  (push (actor-vy actor) (actor-pstack actor))
  (incf (machine-ip machine)))

(defun vm/setv (actor machine shooter)
  (let ((vx (pop (actor-pstack actor)))
        (vy (pop (actor-pstack actor))))
    (setf (actor-vx actor) vx
          (actor-vy actor) vy)
    (incf (machine-ip machine))))

(defun vm/vanish (actor machine shooter)
  (actor-vanish actor)
  (incf (machine-ip machine)))

(defun vm/shot (actor machine shooter)
  (let ((b (find nil (shooter-actors shooter) :key #'actor-used)))
    (when b
      (let ((vy (pop (actor-pstack actor)))
            (vx (pop (actor-pstack actor)))
            (gargs (pop (actor-pstack actor)))
            (args (pop (actor-pstack actor)))
            (code (pop (actor-pstack actor))))
        (setf (actor-used b) t
              (actor-tick actor) 0)
        (setf (actor-x b) (actor-x actor)
              (actor-y b) (actor-y actor))
        (setf (actor-vx b) vx
              (actor-vy b) vy)
        (setf (actor-code b) code
              (actor-pstack b) (reverse args)
              (actor-gstack b) (reverse gargs))))
    (incf (machine-ip machine))))

(defun eval-object (shooter actor)
  (let ((m (make-machine :code (actor-code actor)
                         :ip 0
                         :cstack ())))
    (loop
      :do (when (>= (machine-ip m) (length (machine-code m)))
            (let ((c (pop (machine-cstack m))))
              (if (listp c)
                  (case (first c)
                    (:if (setf (machine-code m) (second c)
                               (machine-ip m) (third c)))
                    (:do (let ((doinfo (second c)))
                           (if (>= (first doinfo) (second doinfo))
                               (progn
                                 (setf (machine-code m) (third c)
                                       (machine-ip m) (fourth c)))
                               (progn
                                 (setf (actor-pstack actor) nil
                                       (machine-ip m) 0)
                                 (push (incf (first (second c)) (third doinfo))
                                       (actor-pstack actor))
                                 (push c (machine-cstack m))))))
                    (otherwise (return-from eval-object)))
                  (return-from eval-object))))
      :do (when (>= (machine-ip m) (length (machine-code m)))
            (return-from eval-object))
      :do (let ((inst (elt (machine-code m) (machine-ip m))))
            (cond ((and (not (null inst)) (symbolp inst))
                   (case inst
                     ;;(:.s (print (actor-pstack actor) #.*standard-output*))
                     ;;; trigonometric
                     ('>rad (vm/>rad actor m shooter))
                     ('>deg (vm/>deg actor m shooter))
                     ('sin (vm/sin actor m shooter))
                     ('cos (vm/cos actor m shooter))
                     ;;; logical
                     ('eq (vm/eq actor m shooter))
                     ('or (vm/or actor m shooter))
                     ('and (vm/and actor m shooter))
                     ;;; numeric
                     ('gt (vm/gt actor m shooter))
                     ('gte (vm/gte actor m shooter))
                     ('lt (vm/lt actor m shooter))
                     ('lte (vm/lte actor m shooter))
                     ('mod (vm/mod actor m shooter))
                     ('add (vm/add actor m shooter))
                     ('sub (vm/sub actor m shooter))
                     ('mul (vm/mul actor m shooter))
                     ('div (vm/div actor m shooter))
                     ('rnd (vm/rnd actor m shooter))
                     ;; lists
                     ('cons (vm/cons actor m shooter))
                     ('car (vm/car actor m shooter))
                     ('cdr (vm/cdr actor m shooter))
                     ;;; vectors
                     ('v/rot (vm/v/rot actor m shooter))
                     ('v/mul (vm/v/mul actor m shooter))
                     ;;; control flow
                     ('if (vm/if actor m shooter))
                     ('do (vm/do actor m shooter))
                     ;;; stack manipulation
                     ('dup (vm/dup actor m shooter))
                     ('drop (vm/drop actor m shooter))
                     ('swap (vm/swap actor m shooter))
                     ('over (vm/over actor m shooter))
                     ('rot (vm/rot actor m shooter))
                     ('<g (vm/<g actor m shooter))
                     ('<<g (vm/<<g actor m shooter))
                     ('>g (vm/>g actor m shooter))
                     ;;; actors
                     ('gtick (vm/gtick actor m shooter))
                     ('atick (vm/atick actor m shooter))
                     ('getp (vm/getp actor m shooter))
                     ('setp (vm/setp actor m shooter))
                     ('getv (vm/getv actor m shooter))
                     ('setv (vm/setv actor m shooter))
                     ;;; bullets
                     ('vanish (vm/vanish actor m shooter))
                     ('shot (vm/shot actor m shooter))))
                  (t (push inst (actor-pstack actor))
                     (incf (machine-ip m))))))))

(let* ((bullet-code `(atick 60 gte
                            ((getp swap drop dup -5 lt swap 300 gt or (vanish) () if
                                   getp drop dup -5 lt swap 550 gt or (vanish) () if)
                             getv 90 >rad v/rot 4 v/mul shot vanish)
                            () if
                            getv 0.965 mul swap 0.965 mul setv))
       (vanish-on-edge `(getp drop dup -10 lt swap 420 gt or (vanish) () if
                              getp swap drop dup -10 lt swap 560 gt or (vanish) () if))
       (code `(;;<<g nil eq (0 0 >g) () if
               ;; atick 5 mod 0 eq
               ;; ((,bullet-code
               ;;   swap dup >rad cos 5 mul swap >
               ;;  <<g <<g 360 add 40 do
               ;; () if
               atick 0 eq
               (((;; P: angle radius
                  ;; G: amp (x . y)
                  over over swap >rad cos swap mul <<g car add
                  rot rot
                  over over swap >rad sin swap mul <<g cdr add
                  >g rot <g
                  setp
                  swap 2 add swap dup 100 gte
                  () (2 add) if
                  atick 10 mod 9 eq
                  (,vanish-on-edge
                   nil nil getp <<g cdr sub 100 div swap <<g car sub 100 div swap shot)
                  () if)
                 swap 1 nil cons cons
                 0 getp cons nil cons cons
                 0 0 shot)
                 0 360 45 do)
               () if
               ;;<g 3.5 add >g
               )))
  (defparameter *enemy-code* code))

(defun make-shooter ()
  (let ((actors ())
        (inputs ())
        (hitables ())
        (animated ())
        (db (make-hash-table)))
    (flet ((make-player ()
             (let ((id (make-entity-id)))
               (push (make-entity db :actor
                                  :id id :type :player :tick 0 :used t
                                  :x 200 :y 500 :px 0 :py 0 :vx 0 :vy 0
                                  :rad 0
                                  :code () :pstack () :gstack ())
                     actors)
               (push (make-entity db :hitable :id id :radius 6) hitables)
               (push (make-entity db :input :id id :u nil :d nil :l nil :r nil :z nil) inputs)
               (push (make-entity db :animated :id id
                                  :idx 0 :num 4
                                  :frame 0 :framenum 10
                                  :name :tapir
                                  :x 20 :y 40 :w 40 :h 60)
                     animated)))
           (make-enemy ()
             (let ((id (make-entity-id)))
               (push (make-entity db :actor
                                  :id id :type :enemy :tick 0 :used t
                                  :x 200 :y 100 :px 0 :py 0 :vx 0 :vy 0
                                  :rad 0
                                  :code *enemy-code* :pstack () :gstack ())
                     actors)
               (push (make-entity db :hitable :id id :radius 6) hitables)
               (push (make-entity db :animated :id id
                                  :idx 0 :num 1
                                  :frame 0 :framenum 5
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
                                  :frame 0 :framenum 5
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
                                  :frame 0 :framenum 3
                                  :name :bullet0-broken
                                  :x 8 :y 8 :w 16 :h 16)
                     animated))))
      (make-player)
      (make-enemy)
      (loop :for _ :from 0 :upto 2000 :do (make-bullet))
      (loop :for _ :from 0 :upto 500 :do (make-particle))
      (make-instance 'shooter
                     :db db
                     :actors (coerce (nreverse actors) 'vector)
                     :inputs (coerce (nreverse inputs) 'vector)
                     :hitables (coerce (nreverse hitables) 'vector)
                     :animated (coerce (nreverse animated) 'vector)))))

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

(defun distance (ax ay bx by)
  (sqrt (+ (expt (- bx ax) 2)
           (expt (- by ay) 2))))

(defun check-collision (shooter)
  (let* ((player (find :player (shooter-actors shooter) :key #'actor-type))
         (ph (get-component shooter (actor-id player) :hitable))
         (pid (actor-id player))
         (px (actor-x player))
         (py (actor-y player)))
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
              (actor-vanish a)
              (let* ((p (find-if (lambda (a)
                                   (and (null (actor-used a))
                                        (eq (actor-type a) :particle)))
                                 (shooter-actors shooter)))
                     (anim (get-component shooter (actor-id p) :animated)))
                (when p
                  (setf (animated-idx anim) 0
                        (animated-frame anim) 0)
                  (setf (actor-used p) t
                        (actor-pstack p) nil
                        (actor-code p) '(atick 19 gte (vanish) () if)
                        (actor-x p) x
                        (actor-y p) y
                        (actor-vx p) (if (zerop (actor-vx a))
                                         (- x (actor-px a))
                                         (actor-vx a))
                        (actor-vy p) (if (zerop (actor-vy a))
                                         (- y (actor-py a))
                                         (actor-vy a))))))))))

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
      :do (when (zerop (mod (animated-frame anim) (animated-framenum anim)))
            (setf (animated-idx anim)
                  (mod (1+ (animated-idx anim)) (animated-num anim))))
      :do (incf (animated-frame anim)))))

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
  (move-actors scene)
  (move-player scene)
  (check-collision scene)
  (eval-objects scene)
  (update-ticks scene)
  (incf (shooter-tick scene)))

(defmethod input ((scene shooter) &rest keys &key &allow-other-keys)
  (apply #'update-inputs `(,scene ,@keys)))

(defstruct image
  w h texture)

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
