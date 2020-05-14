(defpackage #:lsd
  (:use #:cl)
  (:export #:main))
(in-package #:lsd)

(defparameter *game*
  (list :title "Lazy Sweet Dream"
        :version (asdf:component-version (asdf:find-system :lsd))
        :width 800
        :height 600))

(defparameter *entity-count* 0)
(defun make-entity-id ()
  (prog1 *entity-count*
    (incf *entity-count*)))

(defstruct point id x y)
(defstruct velocity id x y)
(defstruct direction id r)
(defstruct tick id tick)
(defstruct otype id name)
(defstruct input id u d l r s z)
(defstruct used id bool)
(defstruct script id code pstack cstack)

(defstruct shooter
  tick
  database
  points vels dirs ticks types inputs used scripts)

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
  (let* ((db (shooter-database shooter))
         (components (gethash (intern (symbol-name ctype) :lsd) db)))
    (unless (null components)
      (gethash entity components))))

(defun eval-object-1 (shooter point vel dir tick otype input used script)
  (let ((code (script-code script))
        (ip 0)
        (cstack ()))
    (loop
      :do (when (>= ip (length code))
            (let ((c (pop cstack)))
              (if (and (not (null c)) (listp c))
                  (progn
                    (setf code (first c)
                          ip (second c)
                          (script-pstack script) (third c))
                    (when (>= ip (length code))
                      (return-from eval-object-1)))
                  (return-from eval-object-1))))
      :do (let* ((c (elt code ip))
                 (inst (if (and (not (null c)) (symbolp c)) (intern (symbol-name c) :keyword) c)))
;;            (print (list ip inst cstack) #.*standard-output*)
            (cond ((and (not (null inst)) (symbolp inst))
                   (case inst
;;                     (:.s (print (script-pstack script) #.*standard-output*))
                     (:d2r (progn
                             (push (/ (* PI (pop (script-pstack script))) 180)
                                   (script-pstack script))
                             (incf ip)))
                     (:r2d (progn
                             (push (/ (* 180 (pop (script-pstack script))) PI)
                                   (script-pstack script))
                             (incf ip)))
                     (:eq (progn
                            (push (equal (pop (script-pstack script))
                                         (pop (script-pstack script)))
                                  (script-pstack script))
                            (incf ip)))
                     (:gt (let ((b (pop (script-pstack script)))
                                (a (pop (script-pstack script))))
                            (push (> a b) (script-pstack script))
                            (incf ip)))
                     (:gte (let ((b (pop (script-pstack script)))
                                 (a (pop (script-pstack script))))
                            (push (>= a b) (script-pstack script))
                            (incf ip)))
                     (:lt (let ((b (pop (script-pstack script)))
                                (a (pop (script-pstack script))))
                            (push (< a b) (script-pstack script))
                            (incf ip)))
                     (:lte (let ((b (pop (script-pstack script)))
                                 (a (pop (script-pstack script))))
                            (push (<= a b) (script-pstack script))
                            (incf ip)))
                     (:mod (progn
                             (push (let ((div (pop (script-pstack script))))
                                     (mod (pop (script-pstack script)) div))
                                   (script-pstack script))
                             (incf ip)))
                     (:tick (progn
                              (push (shooter-tick shooter) (script-pstack script))
                              (incf ip)))
                     (:if (let* ((false-clause (pop (script-pstack script)))
                                 (true-clause (pop (script-pstack script)))
                                 (value (pop (script-pstack script))))
                            (push (list code (1+ ip) (script-pstack script)) cstack)
                            (if value
                                (setf code true-clause
                                      (script-pstack script) nil
                                      ip 0)
                                (setf code false-clause
                                      (script-pstack script) nil
                                      ip 0))))
                     (:dup (let ((a (pop (script-pstack script))))
                             (push a (script-pstack script))
                             (push a (script-pstack script))
                             (incf ip)))
                     (:swap (let ((a (pop (script-pstack script)))
                                  (b (pop (script-pstack script))))
                              (push a (script-pstack script))
                              (push b (script-pstack script))
                              (incf ip)))
                     (:add (progn
                             (push (+ (pop (script-pstack script))
                                      (pop (script-pstack script)))
                                   (script-pstack script))
                             (incf ip)))
                     (:sub (let ((b (pop (script-pstack script)))
                                 (a (pop (script-pstack script))))
                             (push (- a b) (script-pstack script))
                             (incf ip)))
                     (:mul (progn
                             (push (* (pop (script-pstack script))
                                      (pop (script-pstack script)))
                                   (script-pstack script))
                             (incf ip)))
                     (:div (let ((b (pop (script-pstack script)))
                                 (a (pop (script-pstack script))))
                             (push (/ a b) (script-pstack script))
                             (incf ip)))
                     (:rnd (progn
                             (push (random 1.0)
                                   (script-pstack script))
                             (incf ip)))
                     (:getp (progn
                              (push (point-x point) (script-pstack script))
                              (push (point-y point) (script-pstack script))
                              (incf ip)))
                     (:setp (let ((y (pop (script-pstack script)))
                                  (x (pop (script-pstack script))))
                              (setf (point-x point) x
                                    (point-y point) y)
                              (incf ip)))
                     (:getv (progn
                              (push (velocity-x vel) (script-pstack script))
                              (push (velocity-y vel) (script-pstack script))
                              (incf ip)))
                     (:setv (let ((vx (pop (script-pstack script)))
                                  (vy (pop (script-pstack script))))
                              (setf (velocity-x vel) vx
                                    (velocity-y vel) vy)
                              (incf ip)))
                     (:shot (let ((u (find nil (shooter-used shooter) :key #'used-bool)))
                              (when u
                                (let* ((id (used-id u))
                                       (p (get-component shooter id :point))
                                       (v (get-component shooter id :velocity))
                                       (s (get-component shooter id :script))
                                       (vy (pop (script-pstack script)))
                                       (vx (pop (script-pstack script)))
                                       (code (pop (script-pstack script))))
                                  (setf (used-bool u) t)
                                  (setf (point-x p) (point-x point)
                                        (point-y p) (point-y point))
                                  (setf (velocity-x v) vx
                                        (velocity-y v) vy)
                                  (setf (script-code s) code)))
                              (incf ip)))))
                  (t (push inst (script-pstack script))
                     (incf ip)))))))

(defun init-shooter ()
  (let ((points ())
        (vels ())
        (dirs ())
        (ticks ())
        (types ())
        (inputs ())
        (scripts ())
        (used ())
        (db (make-hash-table)))
    (flet ((make-player ()
             (let ((id (make-entity-id)))
               (push (make-entity db :otype :id id :name :player) types)
               (push (make-entity db :used :id id :bool t) used)
               (push (make-entity db :point :id id :x 400 :y 400) points)
               (push (make-entity db :direction :id id :r 0) dirs)
               (push (make-entity db :input :id id :u nil :d nil :l nil :r nil :z nil) inputs)
               (push (make-entity db :script :id id :code () :pstack ()) scripts)))
           (make-enemy ()
             (let ((id (make-entity-id))
                   (code '(tick 10 mod 0 eq (() rnd rnd shot) () if)))
               (push (make-entity db :otype :id id :name :enemy) types)
               (push (make-entity db :tick :id id :tick -1) ticks)
               (push (make-entity db :used :id id :bool t) used)
               (push (make-entity db :point :id id :x 400 :y 200) points)
               (push (make-entity db :direction :id id :r 0) dirs)
               (push (make-entity db :script :id id :code code :pstack ()) scripts)))
           (make-bullet ()
             (let ((id (make-entity-id)))
               (push (make-entity db :otype :id id :name :bullet) types)
               (push (make-entity db :tick :id id :tick -1) ticks)
               (push (make-entity db :used :id id :bool nil) used)
               (push (make-entity db :point :id id
                                  :x (random 800)
                                  :y (random 600))
                     points)
               (push (make-entity db :velocity :id id
                                  :x (- (random 10) 5)
                                  :y (- (random 10) 5))
                     vels)
               (push (make-entity db :direction :id id :r 0) dirs)
               (push (make-entity db :script :id id :code () :pstack ()) scripts))))
      (make-player)
      (make-enemy)
      (loop :for _ :from 0 :upto 1000 :do (make-bullet))
      (make-shooter :tick 0
                    :database db
                    :points (coerce points 'vector)
                    :vels (coerce vels 'vector)
                    :dirs (coerce dirs 'vector)
                    :ticks (coerce ticks 'vector)
                    :types (coerce types 'vector)
                    :inputs (coerce inputs 'vector)
                    :scripts (coerce scripts 'vector)
                    :used (coerce used 'vector)))))

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
  (let* ((player (find :player (shooter-types shooter) :key #'otype-name))
         (p (get-component shooter (otype-id player) :point))
         (i (get-component shooter (otype-id player) :input)))
    (let ((move (if (input-s i) 3 5)))
      (when (input-u i) (incf (point-y p) (- move)))
      (when (input-d i) (incf (point-y p) move))
      (when (input-l i) (incf (point-x p) (- move)))
      (when (input-r i) (incf (point-x p) move)))))

(defun move-points (shooter)
  (loop
    :for p :across (shooter-points shooter)
    :do (let ((v (get-component shooter (point-id p) :velocity)))
          (unless (null v)
            (setf (point-x p) (+ (point-x p) (velocity-x v))
                  (point-y p) (+ (point-y p) (velocity-y v)))))))

(defun draw-points (shooter renderer)
  (sdl2:set-render-draw-color renderer 255 255 255 255)
  (loop
    :for p :across (shooter-points shooter)
    :when (let ((used (get-component shooter (point-id p) :used)))
            (and (not (null used)) (used-bool used)))
    :do (sdl2:render-draw-rect renderer
                               (sdl2:make-rect (- (floor (point-x p)) 3)
                                               (- (floor (point-y p)) 3)
                                               6 6))))

(defun eval-objects (shooter)
  (loop
    :for u :across (shooter-used shooter)
    :for id := (used-id u)
    :for p := (get-component shooter id :point)
    :for v := (get-component shooter id :velocity)
    :for d := (get-component shooter id :direction)
    :for tick := (get-component shooter id :tick)
    :for type := (get-component shooter id :otype)
    :for input := (get-component shooter id :input)
    :for script := (get-component shooter id :script)
    :do (eval-object-1 shooter p v d tick type input u script)))

(defun shot-bullets (shooter)
  (let* ((e (find :enemy (shooter-types shooter) :key #'otype-name))
         (p (get-component shooter (otype-id e) :point)))
    (let ((n 100))
      (loop
        :for i := n :then (decf i)
        :while (plusp i)
        :do (let ((u (find nil (shooter-used shooter) :key #'used-bool)))
              (unless (null u)
                (setf (used-bool u) t)
                (let ((bp (get-component shooter (used-id u) :point))
                      (bv (get-component shooter (used-id u) :velocity)))
                  (unless (or (null bp) (null bv))
                    (setf (point-x bp) (point-x p)
                          (point-y bp) (point-y p)
                          (velocity-x bv) (* 5 (cos (* 2 pi (/ i n))))
                          (velocity-y bv) (* 5 (sin (* 2 pi (/ i n)))))))))))))

(defun main ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title (getf *game* :title)
                           :w (getf *game* :width)
                           :h (getf *game* :height))
      (sdl2:with-renderer (renderer win :index -1 :flags '(:accelerated))
        (let ((screen-rect (sdl2:make-rect 0 0 (getf *game* :width) (getf *game* :height)))
              (shooter (init-shooter)))
          (sdl2:with-event-loop (:method :poll)
            (:keydown (:keysym keysym)
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-up)
               (update-inputs shooter :u t))
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-down)
               (update-inputs shooter :d t))
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-left)
               (update-inputs shooter :l t))
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-right)
               (update-inputs shooter :r t))
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-lshift)
               (update-inputs shooter :s t))
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-z)
               (update-inputs shooter :z t))
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
               (sdl2:push-event :quit)))
            (:keyup (:keysym keysym)
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-up)
               (update-inputs shooter :u nil))
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-down)
               (update-inputs shooter :d nil))
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-left)
               (update-inputs shooter :l nil))
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-right)
               (update-inputs shooter :r nil))
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-lshift)
               (update-inputs shooter :s nil))
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-z)
               (update-inputs shooter :z nil)))
            (:idle ()
             (sdl2:set-render-draw-color renderer 40 40 40 255)
             (sdl2:render-fill-rect renderer screen-rect)

             (draw-points shooter renderer)
             (move-points shooter)
             (move-player shooter)
             (eval-objects shooter)

             (incf (shooter-tick shooter))
             (sdl2:render-present renderer)

             (sdl2:delay (floor (/ 1000 60))))
            (:quit () t)))))))
