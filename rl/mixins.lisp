(in-package #:rl)

(defclass cooldown ()
  ((%cooldown :initform 0 :accessor cooldown)))

(defclass attacking ()
  ((%current-windup :initform 0 :accessor current-windup)
   (%attacking-pos :accessor attacking-pos)))

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

(defclass right-arm ()
  ((%equip-right-arm :initarg :equip-right-arm :initform nil :accessor equip-right-arm)))

(defclass left-arm ()
  ((%equip-left-arm :initarg :equip-left-arm :initform nil :accessor equip-left-arm)))

(defclass blocking ()
  ())

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

(defclass alive (moveable)
  ((%vitality :initform (error "vitality must be set") :accessor vitality)
   (%strength :initform (error "strength must be set") :accessor strength)
   (%dexterity :initform (error "dexterity must be set") :accessor dexterity)
   (%endurance :initform (error "endurance must be set") :accessor endurance)

   (%strength-damage :reader strength-damage)
   (%dexterity-damage :reader dexterity-damage)

   (%stamina :reader stamina)
   (%max-stamina :reader max-stamina)
   (%previous-stamina :accessor previous-stamina :initform 0)
   (%stamina-recharging :accessor stamina-recharging :initform nil)

   (%health :reader health)
   (%max-health :reader max-health)
   (%previous-health :accessor previous-health :initform 0)
   (%resistances :initform '() :initarg :resistances :accessor resistances)))

(defmethod calculate-max-health ((obj alive))
  (round (+ 90 (* 100 (/ (vitality obj) 50)))))

(defmethod calculate-max-stamina ((obj alive))
  (round (+ 90 (* 100 (/ (endurance obj) 50)))))

(defmethod initialize-instance :after ((obj alive) &key)
  (setf (slot-value obj '%max-health) (calculate-max-health obj)
        (slot-value obj '%max-stamina) (calculate-max-stamina obj)
        (slot-value obj '%health) (max-health obj)
        (slot-value obj '%stamina) (max-stamina obj)))

(defmethod (setf health) (new-value (obj alive))
  (when (< new-value (health obj))
    (setf (slot-value obj '%previous-health) (health obj)))
  (setf (slot-value obj '%health) (min (max-health obj) new-value)))

(defmethod (setf stamina) (new-value (obj alive))
  (when (< new-value (stamina obj))
    (setf (previous-stamina obj) (stamina obj)))
  (setf (slot-value obj '%stamina) (min (max-stamina obj) new-value)))

(defclass humanoid (inventory right-arm left-arm)
  ())

(defun make-resistance (type &optional amount)
  (let ((resistance (make-instance 'resistance :resistance-to (find-class type))))
    (when amount
      (setf (resistance-amount resistance) amount))
    resistance))

(defparameter *running-stamina* 3)

(defmethod update :after ((obj moveable))
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

    (when (and (typep obj 'alive) (typep obj 'running))
      (cond ((< (stamina obj) *running-stamina*)
             (delete-from-mix obj 'running))
            (t (decf (stamina obj) *running-stamina*))))

    (setf dx (- dx (* dx friction))
          dy (- dy (* dy friction)))))

(defmethod update :after ((obj alive))
  (when (> (previous-health obj) (health obj))
    (decf (previous-health obj) 5)))

(defmethod update :around ((obj alive))
  (let ((start-stamina (stamina obj)))
    (call-next-method)
    (unless (or (< (stamina obj) start-stamina) (typep obj 'blocking))
      (let ((increase (if (and (typep obj 'cooldown) (not (zerop (cooldown obj))))
                          (cooldown obj)
                          1)))
        (setf (stamina obj) (min (+ (stamina obj) increase) (max-stamina obj)))))))

(defmethod cool-down ((obj cooldown))
  (when (plusp (cooldown obj))
    (decf (cooldown obj))))

(defun cooling-down-p (obj)
  (and (typep obj 'cooldown) (plusp (cooldown obj))))

(defun attacking-p (obj)
  (typep obj 'attacking))

(defmethod progress-attack ((obj attacking))
  (if (plusp (current-windup obj))
      (decf (current-windup obj))
      (attack (get-object-at-pos (attacking-pos obj)) obj)))

(defun get-modifiers (obj)
  (remove-if (op (or (typep _1 'mixin-class)
                     (eq _1 (find-class 'modifier))
                     (not (c2mop:subtypep _1 (find-class 'modifier)))))
             (c2mop:class-precedence-list (class-of obj))))

(defmethod toggle-shield ((obj left-arm))
  (cond ((typep obj 'blocking)
         (write-to-log "~a lowered ~a"
                       (display-name obj)
                       (display-name (equip-left-arm obj)))
         (delete-from-mix obj 'blocking))
        ((equip-left-arm obj)
         (write-to-log "~a raised ~a"
                       (display-name obj)
                       (display-name (equip-left-arm obj)))
         (ensure-mix obj 'blocking))
        (t (write-to-log "~a doesn't have a shield equipped" (display-name obj)))))
