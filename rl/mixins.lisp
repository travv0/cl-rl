(in-package #:rl)

(defvar *display-function*
  (lambda (x y char fg-color bg-color bold)
    (declare (ignore x y char fg-color bg-color bold))
    (error "*display-function* must be set to a function that draws entities"))
  "function that displays entites")

(defclass cooldown ()
  ((%cooldown :initform 0 :accessor cooldown)))

(defclass moveable (pos cooldown)
  ((%dx :initarg :dx :initform 0 :accessor dx)
   (%dy :initarg :dy :initform 0 :accessor dy)
   (%friction :initarg :friction :initform 1 :accessor friction)
   (%move-cooldown :initarg :move-cooldown :initform 5 :accessor move-cooldown)))

(defclass visible (pos)
  ((%char :initarg :char :accessor display-char)
   (%foreground-color :initarg :fg-color :accessor foreground-color :initform :white)
   (%background-color :initarg :bg-color :accessor background-color :initform :black)
   (%bold-color :initarg :bold :accessor bold-color :initform t)))

(defclass can-see ()
  ())

(defclass solid () ())

(defclass inventory ()
  ((%inventory :initarg :inventory :initform '() :accessor inventory)))

(defclass resistance ()
  ((%resistance-to :initarg :resistance-to
                   :initform (error "resistence-to must be specified")
                   :accessor resistance-to)
   (%resistance-amount :initarg :resistance-amount
                       :initform 1.2
                       :accessor resistance-amount)))

(defclass health ()
  ((%health :initarg :health :initform 100 :accessor health)
   (%resistances :initform '() :initarg :resistances :accessor resistances)))

(defclass equip-weapon ()
  ())

(defclass right-arm (equip-weapon)
  ((%equip-right-arm :initarg :equip-right-arm :initform nil :accessor equip-right-arm)))

(defclass left-arm (equip-weapon)
  ((%equip-left-arm :initarg :equip-left-arm :initform nil :accessor equip-left-arm)))

(defclass damage ()
  ((%damage :initarg :damage :initform 30 :accessor damage)))

(defclass modifier ()
  ())

(defclass fire (modifier)
  ())

(defclass ice (modifier)
  ())

(defclass deleted ()
  ())

(defclass running (pos)
  ())

(defclass opaque ()
  ())

(defun make-resistance (type &optional amount)
  (let ((resistance (make-instance 'resistance :resistance-to (find-class type))))
    (when amount
      (setf (resistance-amount resistance) amount))
    resistance))

(defmethod update :before ((obj moveable))
  (with-accessors ((x x) (y y)
                   (dx dx) (dy dy)
                   (friction friction)
                   (move-cooldown move-cooldown)
                   (cooldown cooldown))
      obj
    (unless (and (zerop dx) (zerop dy))
      (loop with collisions = (sort (check-collisions obj) #'< :key (op (distance obj (cdr _))))
            for collision in collisions do
              (destructuring-bind (other-obj . last-pos) collision
                (when (or (and (typep obj 'solid) (typep other-obj 'solid))
                          (and other-obj (typep obj 'running) (typep other-obj 'item)))
                  (setf dx 0 dy 0)
                  (update-pos obj (x last-pos) (y last-pos)))
                (unless (typep obj 'running)
                  (collide other-obj obj))
                (when (and (zerop dx) (zerop dy))
                  (delete-from-mix obj 'running)
                  (return))))
      (let ((move-cooldown (if (typep obj 'running)
                               (round (* move-cooldown 2/3))
                               move-cooldown)))
        (setf cooldown move-cooldown)))
    (update-pos obj (+ x (round dx)) (+ y (round dy)))
    (when (and (typep obj 'running)
               (wall-different-p obj (- x (round dx)) (- y (round dy))))
      (setf dx 0 dy 0)
      (delete-from-mix obj 'running))
    (unless (typep obj 'running)
      (setf dx (- dx (* dx friction))
            dy (- dy (* dy friction))))))

(defmethod cool-down ((obj cooldown))
  (when (plusp (cooldown obj))
    (decf (cooldown obj))))

(defmethod display ((obj visible) x y)
  (funcall *display-function*
           x
           y
           (display-char obj)
           (foreground-color obj)
           (background-color obj)
           (bold-color obj)))

(defun cooling-down-p (obj)
  (and (typep obj 'cooldown) (plusp (cooldown obj))))

(defun get-modifiers (obj)
  (remove-if (op (or (typep _1 'mixin-class)
                     (eq _1 (find-class 'modifier))
                     (not (c2mop:subtypep _1 (find-class 'modifier)))))
             (c2mop:class-precedence-list (class-of obj))))
