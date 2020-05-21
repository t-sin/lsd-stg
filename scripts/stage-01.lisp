(defparameter *enemy01*
  (let* ((vanish-on-edge `(;; vanish itsself when out of screen
                           pos drop dup -10 lt swap 420 gt or (vanish) () if
                           pos swap drop dup -10 lt swap 560 gt or (vanish) () if))
         (shot-5-way-to-player `(;; n-way
                                 >g
                                 ;; bullets will be vanish on edge of screen
                                 ,vanish-on-edge () nil nil
                                 ;; aim to playter
                                 ppos pos -1 v/mul v/add atan dup
                                 @g >rad add cos 4 mul swap
                                 <g >rad add sin 4 mul
                                 shot))
         (shot-two-bits `(;; shot two bits.
                          (;; if the bits' speed is greater than 0.1 then decrease its speed otherwise stops
                           vel v/norm 0.1 gt (vel 0.94 v/mul vel!) (0 0 vel!) if
                           atick 30 gte atick 20 mod 0 eq and
                           ;; but else the bits shot to player n-way bullets.
                           (,shot-5-way-to-player -16 16 8 do) () if) swap () swap
                          nil swap nil swap
                          8 mul 1 shot))
         (code `(;; at once,
                 atick 0 eq (swtime ,shot-two-bits -1 1 2 do) () if
                 ;; by 10 frames
                 atick 60 mod 0 eq
                 (;; shot arround 90-way bullets
                  (dup
                   >rad cos 2.3 mul >g
                   >rad sin 2.3 mul >g
                   ,vanish-on-edge nil nil <g <g swap shot)
                  rnd 15 mul dup 360 add 4 do)
                 () if)))
    code))

(vector
 (cons 0 ($put :player 200 500 0 0 () () () ()))
 (cons 10 ($put :enemy 200 100 0 0 () () *enemy01* '(swtime)))
 (cons 120 ($put :bullet 100 20 0 1 () () () ()))
 (cons 130 ($put :bullet 200 20 0 1 () () () ()))
 (cons 140 ($put :bullet 300 20 0 1 () () () ()))
 (cons 150 ($put :bullet 400 20 0 1 () () () ())))
