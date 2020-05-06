(in-package #:rl)

(defclass cooldown ()
  ((%cooldown :initform 0 :accessor cooldown)))

(defclass moveable (pos cooldown)
  ((%dx :initarg :dx :initform 0 :accessor dx)
   (%dy :initarg :dy :initform 0 :accessor dy)
   (%friction :initarg :friction :initform 1 :accessor friction)
   (%move-cooldown :initarg :move-cooldown :initform 5 :accessor move-cooldown)))

(defclass visible (pos)
  ())

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
   (%max-health :initarg :max-health :accessor max-health)
   (%resistances :initform '() :initarg :resistances :accessor resistances)))

(defmethod initialize-instance :after ((health health) &key)
  (unless (slot-boundp health '%max-health)
    (setf (max-health health) (health health))))

(defclass stamina ()
  ((%stamina :initarg :stamina :initform 100 :reader stamina)
   (%max-stamina :initarg :max-stamina :accessor max-stamina)
   (%previous-stamina :reader previous-stamina :initform 0)
   (%stamina-recharging :accessor stamina-recharging :initform nil)))

(defmethod initialize-instance :after ((stamina stamina) &key)
  (unless (slot-boundp stamina '%max-stamina)
    (setf (max-stamina stamina) (stamina stamina))))

(defmethod (setf stamina) (new-value (stamina stamina))
  (when (< new-value (stamina stamina))
    (setf (slot-value stamina '%previous-stamina) (stamina stamina)))
  (setf (slot-value stamina '%stamina) new-value))

(defclass equip-weapon ()
  ())

(defclass right-arm (equip-weapon)
  ((%equip-right-arm :initarg :equip-right-arm :initform nil :accessor equip-right-arm)))

(defclass left-arm (equip-weapon)
  ((%equip-left-arm :initarg :equip-left-arm :initform nil :accessor equip-left-arm)))

(defclass damage ()
  ((%damage :initarg :damage :initform (error "damage must be set") :accessor damage)))

(defclass stamina-use ()
  ((%stamina-use :initarg :damage :accessor stamina-use)))

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

(defparameter *running-stamina* 3)

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
                (when (or (and (typep obj 'solid) (typep other-obj 'solid)))
                  (setf dx 0 dy 0)
                  (update-pos obj (x last-pos) (y last-pos)))
                (collide other-obj obj)
                (when (and (zerop dx) (zerop dy))
                  (delete-from-mix obj 'running)
                  (return))))

      (unless (and (zerop dx) (zerop dy))
        (let ((move-cooldown (if (typep obj 'running)
                                 (round (* move-cooldown (if (< (stamina obj) *running-stamina*)
                                                             2
                                                             2/3)))
                                 move-cooldown)))
          (incf cooldown move-cooldown))))

    (update-pos obj (+ x (round dx)) (+ y (round dy)))

    (when (and (typep obj 'stamina) (typep obj 'running))
      (cond ((< (stamina obj) *running-stamina*)
             (delete-from-mix obj 'running))
            (t (decf (stamina obj) *running-stamina*))))

    (setf dx (- dx (* dx friction))
          dy (- dy (* dy friction)))))

(defmethod update :around ((obj stamina))
  (let ((start-stamina (stamina obj)))
    (call-next-method)
    (unless (< (stamina obj) start-stamina)
      (let ((increase (if (and (typep obj 'cooldown) (not (zerop (cooldown obj))))
                          (cooldown obj)
                          1)))
        (setf (stamina obj) (min (+ (stamina obj) increase) (max-stamina obj)))))))

(defmethod cool-down ((obj cooldown))
  (when (plusp (cooldown obj))
    (decf (cooldown obj))))

(defun cooling-down-p (obj)
  (and (typep obj 'cooldown) (plusp (cooldown obj))))

(defun get-modifiers (obj)
  (remove-if (op (or (typep _1 'mixin-class)
                     (eq _1 (find-class 'modifier))
                     (not (c2mop:subtypep _1 (find-class 'modifier)))))
             (c2mop:class-precedence-list (class-of obj))))
