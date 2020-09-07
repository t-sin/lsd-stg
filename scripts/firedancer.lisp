(defparameter *vanish-on-edge*
  '(;; vanish itsself when out of screen
    pos drop dup -10 lt swap 420 gt or (vanish) () if
    pos swap drop dup -10 lt swap 560 gt or (vanish) () if))

(defparameter *enemy01*
  (let* ((bit-code `(360 180 div add dup @g car add dup >rad
                       dup cos 80 mul swap sin 80 mul
                       @g cdr dup car swap cdr swap v/add pos!
                       dup 4 mul @g car add >rad >g
                       ((,@*vanish-on-edge* vel 1.014 v/mul vel!) () () ()
                        <g dup cos 0.8 mul swap sin 0.8 mul shot) (<g drop) if))
         (shot-two-bits `(>g (,@*vanish-on-edge* ,@bit-code) ()
                             0 () cons
                             <g 180 mul pos cons cons () cons
                             0 0 shot))
         (code `(vel v/norm 0.1 gt (vel 0.93 v/mul vel!) (0 0 vel!) if
                     atick 70 eq (,shot-two-bits 0 1 1 do) () if)))
  code))

(vector
 (cons 0 ($put :player 200 500 0 0 () () () ()))
 (cons 10 ($put :enemy 200 -100 0 20 () () *enemy01* ())))



