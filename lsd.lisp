(defpackage #:lsd
  (:use #:cl)
  (:export #:main))
(in-package #:lsd)

(defstruct game
  (title "Lazy Sweet Dream")
  (version (asdf:component-version (asdf:find-system :lsd)))
  (width 800)
  (height 600)
  scene)

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

(defparameter *entity-count* 0)
(defun make-entity-id ()
  (prog1 *entity-count*
    (incf *entity-count*)))

(defstruct actor
  id type tick
  x y
  vx vy
  rad
  used
  code pstack gstack)

(defstruct input id u d l r s z)

(defun make-entity (db type &rest keys &key &allow-other-keys)
  (let* ((maker-name (intern (format nil "MAKE-~a" (symbol-name type)) :lsd))
         (maker (symbol-function maker-name))
         (id-name (intern (format nil "~a-ID" (symbol-name type)) :lsd))
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
         (components (gethash (intern (symbol-name ctype) :lsd) db)))
    (unless (null components)
      (gethash entity components))))

(defun eval-object (shooter actor)
  (let ((code (actor-code actor))
        (ip 0)
        (cstack ()))
    (loop
      :do (when (>= ip (length code))
            (let ((c (pop cstack)))
              (if (listp c)
                  (case (first c)
                    (:if (setf code (second c)
                               ip (third c)))
                    (:do (let ((doinfo (second c)))
                           (if (> (first doinfo) (second doinfo))
                               (setf code (third c)
                                     ip (fourth c))
                               (progn
                                 (setf (actor-pstack actor) nil
                                       ip 0)
                                 (push (incf (first (second c)) (third doinfo))
                                       (actor-pstack actor))
                                 (push c cstack)))))
                    (otherwise (return-from eval-object)))
                  (return-from eval-object))))
      :do (when (>= ip (length code))
            (return-from eval-object))
      :do (let* ((c (elt code ip))
                 (inst (if (and (not (null c)) (symbolp c)) (intern (symbol-name c) :keyword) c)))
            ;;            (print (list ip inst cstack) #.*standard-output*)
            (cond ((and (not (null inst)) (symbolp inst))
                   (case inst
                     ;;(:.s (print (actor-pstack actor) #.*standard-output*))
                     ;;; trigonometric
                     (:>rad (progn
                              (push (/ (* PI (pop (actor-pstack actor))) 180) (actor-pstack actor))
                              (incf ip)))
                     (:>deg (progn
                              (push (/ (* 180 (pop (actor-pstack actor))) PI) (actor-pstack actor))
                              (incf ip)))
                     (:sin (progn
                             (push (sin (pop (actor-pstack actor))) (actor-pstack actor))
                             (incf ip)))
                     (:cos (progn
                             (push (cos (pop (actor-pstack actor))) (actor-pstack actor))
                             (incf ip)))
                     ;;; logical
                     (:eq (progn
                            (push (equal (pop (actor-pstack actor)) (pop (actor-pstack actor))) (actor-pstack actor))
                            (incf ip)))
                     (:or (let ((b (pop (actor-pstack actor)))
                                (a (pop (actor-pstack actor))))
                            (push (or a b) (actor-pstack actor))
                            (incf ip)))
                     (:and (let ((b (pop (actor-pstack actor)))
                                 (a (pop (actor-pstack actor))))
                             (push (and a b) (actor-pstack actor))
                             (incf ip)))
                     ;;; numeric
                     (:gt (let ((b (pop (actor-pstack actor)))
                                (a (pop (actor-pstack actor))))
                            (push (> a b) (actor-pstack actor))
                            (incf ip)))
                     (:gte (let ((b (pop (actor-pstack actor)))
                                 (a (pop (actor-pstack actor))))
                             (push (>= a b) (actor-pstack actor))
                             (incf ip)))
                     (:lt (let ((b (pop (actor-pstack actor)))
                                (a (pop (actor-pstack actor))))
                            (push (< a b) (actor-pstack actor))
                            (incf ip)))
                     (:lte (let ((b (pop (actor-pstack actor)))
                                 (a (pop (actor-pstack actor))))
                             (push (<= a b) (actor-pstack actor))
                             (incf ip)))
                     (:mod (progn
                             (push (let ((div (pop (actor-pstack actor))))
                                     (mod (pop (actor-pstack actor)) div))
                                   (actor-pstack actor))
                             (incf ip)))
                     (:add (progn
                             (push (+ (pop (actor-pstack actor))
                                      (pop (actor-pstack actor)))
                                   (actor-pstack actor))
                             (incf ip)))
                     (:sub (let ((b (pop (actor-pstack actor)))
                                 (a (pop (actor-pstack actor))))
                             (push (- a b) (actor-pstack actor))
                             (incf ip)))
                     (:mul (progn
                             (push (* (pop (actor-pstack actor))
                                      (pop (actor-pstack actor)))
                                   (actor-pstack actor))
                             (incf ip)))
                     (:div (let ((b (pop (actor-pstack actor)))
                                 (a (pop (actor-pstack actor))))
                             (push (/ a b) (actor-pstack actor))
                             (incf ip)))
                     (:rnd (progn
                             (push (random 1.0)
                                   (actor-pstack actor))
                             (incf ip)))
                     ;;; lists
                     (:cons (let ((b (pop (actor-pstack actor)))
                                  (a (pop (actor-pstack actor))))
                              (push (cons a b) (actor-pstack actor))
                              (incf ip)))
                     (:car (progn
                             (push (car (pop (actor-pstack actor)))
                                   (actor-pstack actor))
                             (incf ip)))
                     (:cdr (progn
                             (push (cdr (pop (actor-pstack actor)))
                                   (actor-pstack actor))
                             (incf ip)))
                     ;;; vectors
                     (:v/rot (let ((theta (pop (actor-pstack actor)))
                                   (y (pop (actor-pstack actor)))
                                   (x (pop (actor-pstack actor))))
                                (push (- (* x (cos theta)) (* y (sin theta)))
                                      (actor-pstack actor))
                                (push (+ (* x (sin theta)) (* y (cos theta)))
                                      (actor-pstack actor))
                                (incf ip)))
                     (:v/mul (let ((a (pop (actor-pstack actor)))
                                   (y (pop (actor-pstack actor)))
                                   (x (pop (actor-pstack actor))))
                               (push (* a x) (actor-pstack actor))
                               (push (* a y) (actor-pstack actor))
                               (incf ip)))
                     ;;; control flow
                     (:if (let* ((false-clause (pop (actor-pstack actor)))
                                 (true-clause (pop (actor-pstack actor)))
                                 (value (pop (actor-pstack actor))))
                            (push (list :if code (1+ ip)) cstack)
                            (if value
                                (setf code true-clause
                                      ip 0)
                                (setf code false-clause
                                      ip 0))))
                     (:do (let ((diff (pop (actor-pstack actor)))
                                (e (pop (actor-pstack actor)))
                                (s (pop (actor-pstack actor)))
                                (proc (pop (actor-pstack actor))))
                            (push (list :do (list s e diff) code (1+ ip)) cstack)
                            (push s (actor-pstack actor))
                            (setf code proc
                                  ip 0)))
                     ;;; stack manipulation
                     (:dup (let ((a (pop (actor-pstack actor))))
                             (push a (actor-pstack actor))
                             (push a (actor-pstack actor))
                             (incf ip)))
                     (:drop (progn
                              (pop (actor-pstack actor))
                              (incf ip)))
                     (:swap (let ((a (pop (actor-pstack actor)))
                                  (b (pop (actor-pstack actor))))
                              (push a (actor-pstack actor))
                              (push b (actor-pstack actor))
                              (incf ip)))
                     (:over (let ((b (pop (actor-pstack actor)))
                                  (a (pop (actor-pstack actor))))
                             (push a (actor-pstack actor))
                             (push b (actor-pstack actor))
                             (push a (actor-pstack actor))
                             (incf ip)))
                     (:rot (let ((c (pop (actor-pstack actor)))
                                 (b (pop (actor-pstack actor)))
                                 (a (pop (actor-pstack actor))))
                             (push b (actor-pstack actor))
                             (push c (actor-pstack actor))
                             (push a (actor-pstack actor))
                             (incf ip)))
                     (:<g (progn
                            (push (pop (actor-gstack actor))
                                  (actor-pstack actor))
                            (incf ip)))
                     (:<<g (let ((a (pop (actor-gstack actor))))
                             (push a (actor-gstack actor))
                             (push a (actor-pstack actor))
                             (incf ip)))
                     (:>g (progn
                            (push (pop (actor-pstack actor))
                                  (actor-gstack actor))
                            (incf ip)))
                     ;;; actors
                     (:gtick (progn
                               (push (shooter-tick shooter) (actor-pstack actor))
                               (incf ip)))
                     (:atick (progn
                               (push (actor-tick actor) (actor-pstack actor))
                               (incf ip)))
                     (:getp (progn
                              (push (actor-x actor) (actor-pstack actor))
                              (push (actor-y actor) (actor-pstack actor))
                              (incf ip)))
                     (:setp (let ((y (pop (actor-pstack actor)))
                                  (x (pop (actor-pstack actor))))
                              (setf (actor-x actor) x
                                    (actor-y actor) y)
                              (incf ip)))
                     (:getv (progn
                              (push (actor-vx actor) (actor-pstack actor))
                              (push (actor-vy actor) (actor-pstack actor))
                              (incf ip)))
                     (:setv (let ((vx (pop (actor-pstack actor)))
                                  (vy (pop (actor-pstack actor))))
                              (setf (actor-vx actor) vx
                                    (actor-vy actor) vy)
                              (incf ip)))
                     ;;; bullets
                     (:vanish (progn
                                (setf (actor-used actor) nil
                                      (actor-tick actor) 0)
                                (incf ip)))
                     (:shot (let ((b (find nil (shooter-actors shooter) :key #'actor-used)))
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
                              (incf ip)))))
                  (t (push inst (actor-pstack actor))
                     (incf ip)))))))

(let* ((bullet-code `(atick 60 gte
                            ((getp swap drop dup -50 lt swap 610 gt or (vanish) () if
                                   getp drop dup -50 lt swap 810 gt or (vanish) () if)
                             getv 90 >rad v/rot 4 v/mul shot vanish)
                            () if
                            getv 0.965 mul swap 0.965 mul setv))
       (code `(;;<<g nil eq (0 0 >g) () if
               ;; atick 5 mod 0 eq
               ;; ((,bullet-code
               ;;   swap dup >rad cos 5 mul swap >
               ;;  <<g <<g 360 add 40 do
               ;; () if
               atick 0 eq
               (((;; P: angle radius
                  ;; G: (x . y)
                  over over swap >rad cos swap mul <<g car add
                  rot rot
                  over over swap >rad sin swap mul <<g cdr add
                  >g rot <g
                  setp
                  swap 2 add swap dup 100 gte (drop 100) (2 add) if
                  atick 10 mod 0 eq ((getp swap drop dup -50 lt swap 610 gt or (vanish) () if
                                      getp drop dup -50 lt swap 810 gt or (vanish) () if)
                                     nil nil getp <<g cdr sub 100 div swap <<g car sub 100 div swap shot) () if)
                 swap 1 nil cons cons
                 getp cons nil cons
                 0 0 shot)
                 0 359 45 do)
               () if
               ;;<g 3.5 add >g
               )))
  (defparameter *enemy-code* code))

(defun make-shooter ()
  (let ((actors ())
        (inputs ())
        (db (make-hash-table)))
    (flet ((make-player ()
             (let ((id (make-entity-id)))
               (push (make-entity db :actor
                                  :id id :type :player :tick 0 :used t
                                  :x 400 :y 400 :vx 0 :vy 0
                                  :rad 0
                                  :code () :pstack () :gstack ())
                     actors)
               (push (make-entity db :input :id id :u nil :d nil :l nil :r nil :z nil) inputs)))
           (make-enemy ()
             (let ((id (make-entity-id)))
               (push (make-entity db :actor
                                  :id id :type :enemy :tick 0 :used t
                                  :x 400 :y 200 :vx 0 :vy 0
                                  :rad 0
                                  :code *enemy-code* :pstack () :gstack ())
                     actors)))
           (make-bullet ()
             (let ((id (make-entity-id)))
               (push (make-entity db :actor
                                  :id id :type :bullet :tick 0 :used nil
                                  :x 0 :y 0 :vx 0 :vy 0
                                  :rad 0
                                  :code () :pstack () :gstack ())
                     actors))))
      (make-player)
      (make-enemy)
      (loop :for _ :from 0 :upto 2000 :do (make-bullet))
      (make-instance 'shooter
                     :db db
                     :actors (coerce actors 'vector)
                     :inputs (coerce inputs 'vector)))))

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
    :do (when (actor-used a)
          (setf (actor-x a) (+ (actor-x a) (actor-vx a))
                (actor-y a) (+ (actor-y a) (actor-vy a))))))

(defun draw-actors (shooter renderer)
  (sdl2:set-render-draw-color renderer 255 255 255 255)
  (loop
    :for a :across (shooter-actors shooter)
    :when (actor-used a)
    :do (sdl2:render-draw-rect renderer
                               (sdl2:make-rect (- (floor (actor-x a)) 3)
                                               (- (floor (actor-y a)) 3)
                                               6 6))))

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

(defclass shooter (scene)
  ((tick :initform 0
         :accessor shooter-tick)
   (database :initarg :db
             :accessor shooter-db)
   (actors :initarg :actors
           :accessor shooter-actors)
   (inputs :initarg :inputs
           :accessor shooter-inputs)))
  
(defmethod draw ((scene shooter) renderer)
  (sdl2:set-render-draw-color renderer 40 40 40 255)
  (sdl2:render-fill-rect renderer
                         (sdl2:make-rect 0 0 (scene-width scene) (scene-height scene)))
  (draw-actors scene renderer))

(defmethod update ((scene shooter))
  (move-actors scene)
  (move-player scene)
  (eval-objects scene)
  (update-ticks scene)
  (incf (shooter-tick scene)))

(defmethod input (scene &rest keys &key &allow-other-keys)
  (apply #'update-inputs `(scene ,@keys)))

(defun main ()
  (let ((game (make-game :scene (make-shooter))))
    (sdl2:with-init (:everything)
      (sdl2:with-window (win :title (game-title game)
                             :w (game-width game)
                             :h (game-height game))
        (sdl2:with-renderer (renderer win :index -1 :flags '(:accelerated))
          (sdl2:with-event-loop (:method :poll)
            (:keydown (:keysym keysym)
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-up)
               (input (game-scene game) :u t))
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-down)
               (input (game-scene game) :d t))
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-left)
               (input (game-scene game) :l t))
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-right)
               (input (game-scene game) :r t))
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-lshift)
               (input (game-scene game) :s t))
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-z)
               (input (game-scene game) :z t))
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
               (sdl2:push-event :quit)))
            (:keyup (:keysym keysym)
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-up)
               (input (game-scene game) :u nil))
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-down)
               (input (game-scene game) :d nil))
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-left)
               (input (game-scene game) :l nil))
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-right)
               (input (game-scene game) :r nil))
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-lshift)
               (input (game-scene game) :s nil))
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-z)
               (input (game-scene game) :z nil)))
            (:idle ()
             (draw (game-scene game) renderer)
             (update (game-scene game))
             (sdl2:render-present renderer)
             (sdl2:delay (floor (/ 1000 60))))
            (:quit () t)))))))
