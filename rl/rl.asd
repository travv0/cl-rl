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
               #:travv0.utils
               #:marshal)
  :components ((:file "package")
               (:file "utils")
               (:file "pos")
               (:file "mixins")
               (:file "level")
               (:file "astar")
               (:file "items")
               (:file "player")
               (:file "enemies")
               (:file "weapons")
               (:file "collisions")
               (:file "rl")))

(asdf:defsystem #:rl/tests
  :pathname "./t/"
  :serial t
  :depends-on (#:rl #:fiveam #:alexandria)
  :components ((:file "test-collisions")))
