;;;; rl.lisp

(in-package #:rl)

(init-ecs)

(defcomponent position (x y z))
(defcomponent velocity (x y z))

(defsys move ((position velocity) (entity))
  (incf (position/x entity) (velocity/x entity))
  (incf (position/y entity) (velocity/y entity))
  (incf (position/z entity) (velocity/z entity)))

(defsys display ((position) (entity))
  (format t "Entity ~a is at (~a, ~a, ~a)"
          entity (position/x entity) (position/y entity) (position/z entity)))

(defparameter *player*
  (add-entity nil
    (position :x 0 :y 0 :z 0)
    (velocity :x 1 :y 1 :z 0)))
