(defpackage #:petroglyph.class
  (:use #:cl
        #:hoard)
  (:export #:image
           #:image-width
           #:image-height))
(in-package #:petroglyph.class)

(defclass image (resource)
  ((width :type integer
          :initarg :width
          :accessor image-width)
   (height :type integer
           :initarg :height
           :accessor image-height)))

(defpackage #:petroglyph.glfw
  (:use #:cl
        #:hoard
        #:petroglyph.class)
  (:export #:set-color
           #:fill-rect
           #:draw-point
           #:draw-image))
(in-package #:petroglyph.glfw)

(defmethod register ((type (eql :image)) name pathname)
  (let ((img (make-instance 'image
                            :name name
                            :type :image
                            :pathname pathname)))
    (hoard:register-by-name img)
    img))

(defmethod load-resource ((res image) context)
  (let* ((img (pngload:load-file (resource-pathname res)))
         (w (pngload:width img))
         (h (pngload:height img)))
    (setf (resource-obj res) img
          (image-width res) w
          (image-height res) h)
    img))

(defpackage #:petroglyph.sdl2
  (:use #:cl
        #:hoard
        #:petroglyph.class)
  (:export #:set-color
           #:fill-rect
           #:draw-image))
(in-package #:petroglyph.sdl2)

(defmethod register ((type (eql :image)) name pathname)
  (let ((img (make-instance 'image
                            :name name
                            :type :image
                            :pathname pathname)))
    (hoard:register-by-name img)
    img))

(defmethod load-resource ((res image) context)
  (sdl2-image:init '(:png))
  (let* ((pathname (asdf:system-relative-pathname :lsd (resource-pathname res)))
         (surface (sdl2-image:load-image pathname))
         (texture (sdl2:create-texture-from-surface context surface))
         (w (sdl2:surface-width surface))
         (h (sdl2:surface-height surface)))
    (setf (resource-obj res) texture
          (image-width res) w
          (image-height res) h)
    (sdl2-image:quit)
    (call-next-method res nil)
    res))

(defun set-color (context r g b a)
  (sdl2:set-render-draw-color context r g b a))

(defun fill-rect (context x y w h)
  (sdl2:render-fill-rect context (sdl2:make-rect x y w h)))

(defun draw-image (context image sx sy sw sh dx dy dw dh)
  (sdl2:render-copy context
                    (resource-obj image)
                    :source-rect (sdl2:make-rect sx sy sw sh)
                    :dest-rect (sdl2:make-rect dx dy dw dh)))

(defpackage #:petroglyph
  (:use #:cl
        #:hoard
        #:petroglyph.class
        #:petroglyph.sdl2)
  (:export #:image
           #:image-width
           #:image-height

           #:set-color
           #:fill-rect
           #:draw-image))
(in-package #:petroglyph)
