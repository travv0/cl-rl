;;;; rl.lisp

(in-package #:rl)

(init-ecs)

(defcomponent position (x y z))
(defcomponent velocity (x y z))

(defcomponent visible (char color))

(defsys move ((position velocity) (e))
  (incf (position/x e) (velocity/x e))
  (incf (position/y e) (velocity/y e))
  (incf (position/z e) (velocity/z e)))

(defvar *display-function*
  (lambda (x y char color)
    (error "*display-function* must be set to a function that draws entities"))
  "function that displays entites")

(defsys display ((position visible) (e))
  (funcall *display-function*
           (position/x e)
           (position/y e)
           (visible/char e)
           (visible/color e)))

(defparameter *player*
  (add-entity nil
    (position :x 0 :y 0 :z 0)
    (velocity :x 1 :y 1 :z 0)
    (visible :char #\@ :color :white)))

(defparameter *systems*
  '(move
    display)
  "a list of all system names in the order they should run")

(defun update (display-function action)
  (format t "Got action: ~a~%" action)
  (let ((*display-function* display-function))
    (mapc #'do-system *systems*)))
