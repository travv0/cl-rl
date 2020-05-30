;;;; rl-sdl2.asd

(asdf:defsystem #:rl-sdl2
    :description "Describe rl here"
    :author "Your Name <your.name@example.com>"
    :license  "Specify license here"
    :version "0.0.1"
    :serial t
    :depends-on (#:sdl2
                 #:sdl2-image
                 #:alexandria
                 #:serapeum
                 #:net.mfiano.lisp.dungen
                 #:dynamic-mixins
                 #:travv0.utils
                 #:cl-fad)
    :components ((:module "rl"
                  :components ((:file "package")
                               (:file "level")
                               (:file "pos")
                               (:file "mixins")
                               (:file "astar")
                               (:file "player")
                               (:file "enemies")
                               (:file "items")
                               (:file "weapons")
                               (:file "collisions")
                               (:file "rl")))
                 (:module "rl-sdl2"
                  :components ((:file "package")
                               (:file "rl-sdl2")))))
