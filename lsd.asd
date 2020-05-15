(defsystem :lsd
  :version "0.1.0"
  :license "MIT"
  :author "Shinichi Tanaka <shinichi.tanaka45@gmail.com>"
  :description "Bullet-hell shooter 'Lazy Sweet Dream'"
  :depends-on ("sdl2"
               "sdl2-image"
               "sdl2-ttf"
               "cl-portaudio")
  :components ((:file "lsd")))
