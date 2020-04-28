;;;; rl.lisp

(in-package #:rl)

(init-ecs)

(defcomponent position (x y z))
(defcomponent velocity (x y z))

(defcomponent friction (amount))

(defcomponent visible (char color))

(defsys move ((position velocity) (e))
  (incf (position/x e) (round (velocity/x e)))
  (incf (position/y e) (round (velocity/y e)))
  (incf (position/z e) (round (velocity/z e))))

(defsys apply-friction ((velocity) (e))
  (decf (velocity/x e) (* (velocity/x e) (friction/amount e)))
  (decf (velocity/y e) (* (velocity/y e) (friction/amount e))))

(defvar *display-function*
  (lambda (x y char color)
    (declare (ignore x y char color))
    (error "*display-function* must be set to a function that draws entities"))
  "function that displays entites")

(defsys display ((position visible) (e))
  (funcall *display-function*
           (position/x e)
           (position/y e)
           (visible/char e)
           (visible/color e)))

(defparameter *player*
  (progn (when *player*
           (remove-entity *player*))
         (add-entity nil
           (position :x 50 :y 20 :z 0)
           (velocity :x 0 :y 0 :z 0)
           (friction :amount 1)
           (visible :char #\@ :color :white))))

(defparameter *systems*
  '(move
    apply-friction
    display)
  "a list of all system names in the order they should run")

(defun update (display-function action)
  (case action
    ((nil))
    (:move-left (setf (velocity/x *player*) -1))
    (:move-up (setf (velocity/y *player*) -1))
    (:move-right (setf (velocity/x *player*) 1))
    (:move-down (setf (velocity/y *player*) 1))
    (:move-up-left (setf (velocity/x *player*) -1 (velocity/y *player*) -1))
    (:move-up-right (setf (velocity/x *player*) 1 (velocity/y *player*) -1))
    (:move-down-left (setf (velocity/x *player*) -1 (velocity/y *player*) 1))
    (:move-down-right (setf (velocity/x *player*) 1 (velocity/y *player*) 1))
    (t (cerror "continue" "unknown action: ~a" action)))

  (let ((*display-function* display-function))
    (mapc #'do-system *systems*)))
