(defpackage #:lsd.shooter.vm
  (:use #:cl
        #:lsd.shooter.util
        #:lsd.shooter.scene
        #:lsd.shooter.component)
  (:export #:vm
           #:vm-code
           #:vm-ip
           #:vm-cstack
           #:eval-object
           ;; instructions
           #:vm/>rad
           #:vm/>deg
           #:vm/sin
           #:vm/cos
           #:vm/atan
           #:vm/eq
           #:vm/or
           #:vm/and
           #:vm/gt
           #:vm/gte
           #:vm/lt
           #:vm/lte
           #:vm/mod
           #:vm/add
           #:vm/sub
           #:vm/mul
           #:vm/div
           #:vm/rnd
           #:vm/cons
           #:vm/car
           #:vm/cdr
           #:vm/v/rot
           #:vm/v/add
           #:vm/v/mul
           #:vm/v/norm
           #:vm/if
           #:vm/do
           #:vm/dup
           #:vm/drop
           #:vm/swap
           #:vm/over
           #:vm/rot
           #:vm/<g
           #:vm/@g
           #:vm/>g
           #:vm/gtick
           #:vm/swtime
           #:vm/atick
           #:vm/ppos
           #:vm/pos
           #:vm/pos!
           #:vm/vel
           #:vm/vel!
           #:vm/vanish
           #:vm/shot
           ;; utilities
           #:shot
           #:put-actor
           #:$put))
(in-package #:lsd.shooter.vm)

(defstruct vm
  code ip cstack)

(defun vm/>rad (actor vm shooter)
  (declare (ignore shooter))
  (push (/ (* PI (pop (actor-pstack actor))) 180)
        (actor-pstack actor))
  (incf (vm-ip vm)))

(defun vm/>deg (actor vm shooter)
  (declare (ignore shooter))
  (push (/ (* 180 (pop (actor-pstack actor))) PI)
        (actor-pstack actor))
  (incf (vm-ip vm)))

(defun vm/sin (actor vm shooter)
  (declare (ignore shooter))
  (push (sin (pop (actor-pstack actor)))
        (actor-pstack actor))
  (incf (vm-ip vm)))

(defun vm/cos (actor vm shooter)
  (declare (ignore shooter))
  (push (cos (pop (actor-pstack actor)))
        (actor-pstack actor))
  (incf (vm-ip vm)))

(defun vm/atan (actor vm shooter)
  (declare (ignore shooter))
  (let ((y (pop (actor-pstack actor)))
        (x (pop (actor-pstack actor))))
    (push (atan y x) (actor-pstack actor))
    (incf (vm-ip vm))))

(defun vm/eq (actor vm shooter)
  (declare (ignore shooter))
  (push (equal (pop (actor-pstack actor))
               (pop (actor-pstack actor)))
        (actor-pstack actor))
  (incf (vm-ip vm)))

(defun vm/or (actor vm shooter)
  (declare (ignore shooter))
  (let ((b (pop (actor-pstack actor)))
        (a (pop (actor-pstack actor))))
    (push (or a b) (actor-pstack actor))
    (incf (vm-ip vm))))

(defun vm/and (actor vm shooter)
  (declare (ignore shooter))
  (let ((b (pop (actor-pstack actor)))
        (a (pop (actor-pstack actor))))
    (push (and a b) (actor-pstack actor))
    (incf (vm-ip vm))))

(defun vm/gt (actor vm shooter)
  (declare (ignore shooter))
  (let ((b (pop (actor-pstack actor)))
        (a (pop (actor-pstack actor))))
    (push (> a b) (actor-pstack actor))
    (incf (vm-ip vm))))

(defun vm/gte (actor vm shooter)
  (declare (ignore shooter))
  (let ((b (pop (actor-pstack actor)))
        (a (pop (actor-pstack actor))))
    (push (>= a b) (actor-pstack actor))
    (incf (vm-ip vm))))

(defun vm/lt (actor vm shooter)
  (declare (ignore shooter))
  (let ((b (pop (actor-pstack actor)))
        (a (pop (actor-pstack actor))))
    (push (< a b) (actor-pstack actor))
    (incf (vm-ip vm))))

(defun vm/lte (actor vm shooter)
  (declare (ignore shooter))
  (let ((b (pop (actor-pstack actor)))
        (a (pop (actor-pstack actor))))
    (push (<= a b) (actor-pstack actor))
    (incf (vm-ip vm))))

(defun vm/mod (actor vm shooter)
  (declare (ignore shooter))
  (push (let ((div (pop (actor-pstack actor))))
          (mod (pop (actor-pstack actor)) div))
        (actor-pstack actor))
  (incf (vm-ip vm)))

(defun vm/add (actor vm shooter)
  (declare (ignore shooter))
  (push (+ (pop (actor-pstack actor))
           (pop (actor-pstack actor)))
        (actor-pstack actor))
  (incf (vm-ip vm)))

(defun vm/sub (actor vm shooter)
  (declare (ignore shooter))
  (let ((b (pop (actor-pstack actor)))
        (a (pop (actor-pstack actor))))
    (push (- a b) (actor-pstack actor))
    (incf (vm-ip vm))))

(defun vm/mul (actor vm shooter)
  (declare (ignore shooter))
  (push (* (pop (actor-pstack actor))
           (pop (actor-pstack actor)))
        (actor-pstack actor))
  (incf (vm-ip vm)))

(defun vm/div (actor vm shooter)
  (declare (ignore shooter))
  (let ((b (pop (actor-pstack actor)))
        (a (pop (actor-pstack actor))))
    (push (/ a b) (actor-pstack actor))
    (incf (vm-ip vm))))

(defun vm/rnd (actor vm shooter)
  (declare (ignore shooter))
  (push (random 1.0)
        (actor-pstack actor))
  (incf (vm-ip vm)))

(defun vm/cons (actor vm shooter)
  (declare (ignore shooter))
  (let ((b (pop (actor-pstack actor)))
        (a (pop (actor-pstack actor))))
    (push (cons a b) (actor-pstack actor))
    (incf (vm-ip vm))))

(defun vm/car (actor vm shooter)
  (declare (ignore shooter))
  (push (car (pop (actor-pstack actor)))
        (actor-pstack actor))
  (incf (vm-ip vm)))

(defun vm/cdr (actor vm shooter)
  (declare (ignore shooter))
  (push (cdr (pop (actor-pstack actor)))
        (actor-pstack actor))
  (incf (vm-ip vm)))

(defun vm/v/rot (actor vm shooter)
  (declare (ignore shooter))
  (let ((theta (pop (actor-pstack actor)))
        (y (pop (actor-pstack actor)))
        (x (pop (actor-pstack actor))))
    (push (- (* x (cos theta)) (* y (sin theta)))
          (actor-pstack actor))
    (push (+ (* x (sin theta)) (* y (cos theta)))
          (actor-pstack actor))
    (incf (vm-ip vm))))

(defun vm/v/add (actor vm shooter)
  (declare (ignore shooter))
  (let ((y2 (pop (actor-pstack actor)))
        (x2 (pop (actor-pstack actor)))
        (y1 (pop (actor-pstack actor)))
        (x1 (pop (actor-pstack actor))))
    (push (+ x1 x2) (actor-pstack actor))
    (push (+ y1 y2) (actor-pstack actor))
    (incf (vm-ip vm))))

(defun vm/v/mul (actor vm shooter)
  (declare (ignore shooter))
  (let ((a (pop (actor-pstack actor)))
        (y (pop (actor-pstack actor)))
        (x (pop (actor-pstack actor))))
    (push (* a x) (actor-pstack actor))
    (push (* a y) (actor-pstack actor))
    (incf (vm-ip vm))))

(defun vm/v/norm (actor vm shooter)
  (declare (ignore shooter))
  (let ((y (pop (actor-pstack actor)))
        (x (pop (actor-pstack actor))))
    (push (distance x y 0 0)
          (actor-pstack actor))
    (incf (vm-ip vm))))

(defun vm/if (actor vm shooter)
  (declare (ignore shooter))
  (let* ((false-clause (pop (actor-pstack actor)))
         (true-clause (pop (actor-pstack actor)))
         (value (pop (actor-pstack actor))))
    (push (list :if
                (vm-code vm)
                (1+ (vm-ip vm)))
          (vm-cstack vm))
    (if value
        (setf (vm-code vm) true-clause
              (vm-ip vm) 0)
        (setf (vm-code vm) false-clause
              (vm-ip vm) 0))))

(defun vm/do (actor vm shooter)
  (declare (ignore shooter))
  (let ((diff (pop (actor-pstack actor)))
        (e (pop (actor-pstack actor)))
        (s (pop (actor-pstack actor)))
        (proc (pop (actor-pstack actor))))
    (push (list :do
                (list s e diff)
                (vm-code vm)
                (1+ (vm-ip vm)))
          (vm-cstack vm))
    (push s (actor-pstack actor))
    (setf (vm-code vm) proc
          (vm-ip vm) 0)))

(defun vm/dup (actor vm shooter)
  (declare (ignore shooter))
  (let ((a (pop (actor-pstack actor))))
    (push a (actor-pstack actor))
    (push a (actor-pstack actor))
    (incf (vm-ip vm))))

(defun vm/drop (actor vm shooter)
  (declare (ignore shooter))
  (pop (actor-pstack actor))
  (incf (vm-ip vm)))

(defun vm/swap (actor vm shooter)
  (declare (ignore shooter))
  (let ((a (pop (actor-pstack actor)))
        (b (pop (actor-pstack actor))))
    (push a (actor-pstack actor))
    (push b (actor-pstack actor))
    (incf (vm-ip vm))))

(defun vm/over (actor vm shooter)
  (declare (ignore shooter))
  (let ((b (pop (actor-pstack actor)))
        (a (pop (actor-pstack actor))))
    (push a (actor-pstack actor))
    (push b (actor-pstack actor))
    (push a (actor-pstack actor))
    (incf (vm-ip vm))))

(defun vm/rot (actor vm shooter)
  (declare (ignore shooter))
  (let ((c (pop (actor-pstack actor)))
        (b (pop (actor-pstack actor)))
        (a (pop (actor-pstack actor))))
    (push b (actor-pstack actor))
    (push c (actor-pstack actor))
    (push a (actor-pstack actor))
    (incf (vm-ip vm))))

(defun vm/<g (actor vm shooter)
  (declare (ignore shooter))
  (push (pop (actor-gstack actor))
        (actor-pstack actor))
  (incf (vm-ip vm)))

(defun vm/@g (actor vm shooter)
  (declare (ignore shooter))
  (let ((a (pop (actor-gstack actor))))
    (push a (actor-gstack actor))
    (push a (actor-pstack actor))
    (incf (vm-ip vm))))

(defun vm/>g (actor vm shooter)
  (declare (ignore shooter))
  (push (pop (actor-pstack actor))
        (actor-gstack actor))
  (incf (vm-ip vm)))

(defun vm/gtick (actor vm shooter)
  (push (shooter-tick shooter) (actor-pstack actor))
  (incf (vm-ip vm)))

(defun vm/swtime (actor vm shooter)
  (declare (ignore actor))
  (if (shooter-tick-enable-p shooter)
      (setf (shooter-tick-enable-p shooter) nil)
      (setf (shooter-tick-enable-p shooter) t))
  (incf (vm-ip vm)))

(defun vm/atick (actor vm shooter)
  (declare (ignore shooter))
  (push (actor-tick actor) (actor-pstack actor))
  (incf (vm-ip vm)))

(defun vm/ppos (actor vm shooter)
  (let ((p (find :player (shooter-actors shooter) :key #'actor-type)))
    (when p
      (push (actor-x p) (actor-pstack actor))
      (push (actor-y p) (actor-pstack actor)))
    (incf (vm-ip vm))))

(defun vm/pos (actor vm shooter)
  (declare (ignore shooter))
  (push (actor-x actor) (actor-pstack actor))
  (push (actor-y actor) (actor-pstack actor))
  (incf (vm-ip vm)))

(defun vm/pos! (actor vm shooter)
  (declare (ignore shooter))
  (let ((y (pop (actor-pstack actor)))
        (x (pop (actor-pstack actor))))
    (setf (actor-px actor) (actor-x actor)
          (actor-py actor) (actor-y actor))
    (setf (actor-x actor) x
          (actor-y actor) y)
    (incf (vm-ip vm))))

(defun vm/vel (actor vm shooter)
  (declare (ignore shooter))
  (push (actor-vx actor) (actor-pstack actor))
  (push (actor-vy actor) (actor-pstack actor))
  (incf (vm-ip vm)))

(defun vm/vel! (actor vm shooter)
  (declare (ignore shooter))
  (let ((vy (pop (actor-pstack actor)))
        (vx (pop (actor-pstack actor))))
    (setf (actor-vx actor) vx
          (actor-vy actor) vy)
    (incf (vm-ip vm))))

(defun vm/vanish (actor vm shooter)
  (declare (ignore shooter))
  (actor-vanish actor)
  (incf (vm-ip vm)))

(defun put-actor (shooter px py vx vy gargs args code type)
  (let ((b (find-if (lambda (a)
                      (and (null (actor-used a))
                           (eq (actor-type a) type)))
                    (shooter-actors shooter))))
    (when b
      (setf (actor-used b) t
            (actor-tick b) 0)
      (setf (actor-x b) px
            (actor-y b) py)
      (setf (actor-vx b) vx
            (actor-vy b) vy)
      (setf (actor-code b) code
            (actor-pstack b) args
            (actor-gstack b) gargs))))

(defun shot (shooter gun vx vy gargs args code type)
  (put-actor shooter
             (actor-x gun) (actor-y gun)
             vx vy
             gargs args code
             type))

(defun vm/shot (actor vm shooter)
  (let ((vy (pop (actor-pstack actor)))
        (vx (pop (actor-pstack actor)))
        (gargs (pop (actor-pstack actor)))
        (args (pop (actor-pstack actor)))
        (code (pop (actor-pstack actor))))
    (shot shooter actor vx vy (nreverse gargs) (nreverse args) code :bullet))
  (incf (vm-ip vm)))

(defun $put (type x y vx vy gargs args code)
  (lambda (shooter)
    (lsd.shooter.vm:put-actor shooter
                              x y vx vy
                              gargs args code
                              type)))

(defun eval-object (shooter actor)
  (let ((vm (make-vm :code (actor-code actor)
                    :ip 0
                    :cstack ())))
    (loop
      :do (when (>= (vm-ip vm) (length (vm-code vm)))
            (let ((c (pop (vm-cstack vm))))
              (if (listp c)
                  (case (first c)
                    (:if (setf (vm-code vm) (second c)
                               (vm-ip vm) (third c)))
                    (:do (let ((doinfo (second c)))
                           (if (>= (first doinfo) (second doinfo))
                               (progn
                                 (setf (vm-code vm) (third c)
                                       (vm-ip vm) (fourth c)))
                               (progn
                                 (setf (actor-pstack actor) nil
                                       (vm-ip vm) 0)
                                 (push (incf (first (second c)) (third doinfo))
                                       (actor-pstack actor))
                                 (push c (vm-cstack vm))))))
                    (otherwise (return-from eval-object)))
                  (return-from eval-object))))
      :do (when (>= (vm-ip vm) (length (vm-code vm)))
            (return-from eval-object))
      :do (let ((inst (elt (vm-code vm) (vm-ip vm))))
            (cond ((and (not (null inst)) (symbolp inst))
                   (case inst
                     ;;(:.s (print (actor-pstack actor) #.*standard-output*))
                     ;;; trigonometric
                     (>rad (vm/>rad actor vm shooter))
                     (>deg (vm/>deg actor vm shooter))
                     (sin (vm/sin actor vm shooter))
                     (cos (vm/cos actor vm shooter))
                     (atan (vm/atan actor vm shooter))
                     ;;; logical
                     (eq (vm/eq actor vm shooter))
                     (or (vm/or actor vm shooter))
                     (and (vm/and actor vm shooter))
                     ;;; numeric
                     (gt (vm/gt actor vm shooter))
                     (gte (vm/gte actor vm shooter))
                     (lt (vm/lt actor vm shooter))
                     (lte (vm/lte actor vm shooter))
                     (mod (vm/mod actor vm shooter))
                     (add (vm/add actor vm shooter))
                     (sub (vm/sub actor vm shooter))
                     (mul (vm/mul actor vm shooter))
                     (div (vm/div actor vm shooter))
                     (rnd (vm/rnd actor vm shooter))
                     ;; lists
                     (cons (vm/cons actor vm shooter))
                     (car (vm/car actor vm shooter))
                     (cdr (vm/cdr actor vm shooter))
                     ;;; vectors
                     (v/rot (vm/v/rot actor vm shooter))
                     (v/mul (vm/v/mul actor vm shooter))
                     (v/add (vm/v/add actor vm shooter))
                     (v/norm (vm/v/norm actor vm shooter))
                     ;;; control flow
                     (if (vm/if actor vm shooter))
                     (do (vm/do actor vm shooter))
                     ;;; stack vmanipulation
                     (dup (vm/dup actor vm shooter))
                     (drop (vm/drop actor vm shooter))
                     (swap (vm/swap actor vm shooter))
                     (over (vm/over actor vm shooter))
                     (rot (vm/rot actor vm shooter))
                     (<g (vm/<g actor vm shooter))
                     (@g (vm/@g actor vm shooter))
                     (>g (vm/>g actor vm shooter))
                     ;;; global states
                     (gtick (vm/gtick actor vm shooter))
                     (swtime (vm/swtime actor vm shooter))
                     ;;; actors
                     (atick (vm/atick actor vm shooter))
                     (ppos (vm/ppos actor vm shooter))
                     (pos (vm/pos actor vm shooter))
                     (pos! (vm/pos! actor vm shooter))
                     (vel (vm/vel actor vm shooter))
                     (vel! (vm/vel! actor vm shooter))
                     ;; bullets
                     (vanish (vm/vanish actor vm shooter))
                     (shot (vm/shot actor vm shooter))
                     (otherwise (error "unkonwn words ~s" inst))))
                  (t (push inst (actor-pstack actor))
                     (incf (vm-ip vm))))))))
