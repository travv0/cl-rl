(in-package #:rl)

(defclass weapon (item damage stamina-use)
  ((%weapon-cooldown :initarg :weapon-cooldown
                     :initform (error "weapon-cooldown must be initialized")
                     :accessor weapon-cooldown)
   (%weapon-windup :initarg :weapon-windup :accessor weapon-windup)))

(defmethod initialize-instance :after ((weapon weapon) &key)
  (unless (slot-boundp weapon '%stamina-use)
    (setf (stamina-use weapon) (+ (damage weapon) 5)))
  (unless (slot-boundp weapon '%weapon-windup)
    (setf (weapon-windup weapon) (floor (weapon-cooldown weapon) 2))))

(defclass dagger (weapon)
  ((%damage :initform 15)
   (%weapon-cooldown :initform 2)))

(defclass sword (weapon)
  ((%damage :initform 25)
   (%weapon-cooldown :initform 4)))

(defclass shield (weapon)
  ((%damage-reduction :initarg :damage-reduction
                      :initform (error "damage-reduction must be initialized")
                      :accessor damage-reduction)
   (%stability :initarg :stability
               :initform (error "stability must be initialized")
               :accessor stability)))

(defclass kite-shield (shield)
  ((%damage :initform 15)
   (%weapon-cooldown :initform 5)
   (%damage-reduction :initform .95)
   (%stability :initform 10)))
