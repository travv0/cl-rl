(asdf:defsystem #:rl
  :description "Describe rl here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:serapeum
               #:str
               #:black-tie
               #:dynamic-mixins
               #:travv0.utils)
  :components ((:file "package")
               (:file "utils")
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
