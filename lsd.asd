(defsystem :lsd
  :version "0.1.0"
  :license "MIT"
  :author "TANAKA Shinichi <shinichi.tanaka45@gmail.com>"
  :description "Bullet-hell shooter 'Lazy Sweet Dream'"
  :depends-on ("cl-glfw3"
               "cl-portaudio")
  :components ((:file "lsd")))
