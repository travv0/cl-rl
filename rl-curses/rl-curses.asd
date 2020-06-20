;;;; rl-curses.asd

(asdf:defsystem #:rl-curses
  :description "Describe rl here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:rl
               #:cl-charms
               #:alexandria
               #:serapeum
               #:travv0.utils)
  :components ((:file "package")
               (:file "rl-curses")))
