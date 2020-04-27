;;;; rl.asd

(asdf:defsystem #:rl
  :description "Describe rl here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ecs)
  :components ((:file "package")
               (:file "rl")))
