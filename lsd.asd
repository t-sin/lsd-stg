(defsystem :lsd
  :version "0.1.0"
  :license "MIT"
  :author "Shinichi Tanaka <shinichi.tanaka45@gmail.com>"
  :description "Bullet-hell shooter 'Lazy Sweet Dream'"
  :depends-on ("alexandria"
               "anaphora"
               "cl-glfw3"
               "cl-opengl"
               "pngload"

               "sdl2"
               "sdl2-image"
               "sdl2-ttf"
               "cl-portaudio")
  :components ((:module "lib"
                :components ((:file "hoard")
                             (:file "petroglyph" :depends-on ("hoard"))
                             (:file "sound" :depends-on ("hoard"))
                             (:file "fauna")))
               (:module "src"
                :depends-on ("lib")
                :serial t
                :components ((:file "scene")
                             (:module "shooter"
                              :serial t
                              :components ((:file "util")
                                           (:file "scene")
                                           (:file "component")
                                           (:file "vm")
                                           (:file "dc")))
                             (:file "shooter-scene")
                             (:file "lsd")))))
