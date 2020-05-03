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
                 #:str)
    :components ((:module "rl"
                  :components ((:file "package")
                               (:file "astar")
                               (:file "rl")))
                 (:module "rl-curses"
                  :components ((:file "package")
                               (:file "rl-curses")))))
