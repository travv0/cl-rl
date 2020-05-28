;;;; rl-curses.asd

(asdf:defsystem #:rl-curses
    :description "Describe rl here"
    :author "Your Name <your.name@example.com>"
    :license  "Specify license here"
    :version "0.0.1"
    :serial t
    :depends-on (#:cl-charms
                 #:gamebox-ecs
                 #:alexandria
                 #:serapeum
                 #:dungen
                 #:dynamic-mixins
                 #:travv0.utils)
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
                 (:module "rl-curses"
                  :components ((:file "package")
                               (:file "rl-curses")))))
