;;;; rl-sdl2.asd

(asdf:defsystem #:rl-sdl2
  :description "Describe rl here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:rl
               #:sdl2
               #:sdl2-image
               #:sdl2-ttf
               #:alexandria
               #:serapeum
               #:travv0.utils
               #:cl-fad)
  :components ((:file "package")
               (:file "rl-sdl2")))
