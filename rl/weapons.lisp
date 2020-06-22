(in-package #:rl)

(defclass weapon (item damage stamina-use)
  ((%weapon-cooldown :initarg :weapon-cooldown
                     :initform (error "weapon-cooldown must be initialized")
                     :accessor weapon-cooldown)
   (%weapon-windup :accessor weapon-windup)
   (%weapon-strength-scale :initform (error "weapon-strength-scale must be initialized")
                           :accessor weapon-strength-scale
                           :type (integer 1 5))
   (%weapon-dexterity-scale :initform (error "weapon-dexterity-scale must be initialized")
                            :accessor weapon-dexterity-scale
                            :type (integer 1 5))))

(defmethod initialize-instance :after ((weapon weapon) &key)
  (unless (slot-boundp weapon '%stamina-use)
    (setf (stamina-use weapon) (+ (floor (damage weapon) 2) 5)))
  (unless (slot-boundp weapon '%weapon-windup)
    (setf (weapon-windup weapon) (floor (weapon-cooldown weapon) 2))))

(defclass dagger (weapon)
  ((%damage :initform 15)
   (%weapon-cooldown :initform 2)
   (%weapon-strength-scale :initform 1)
   (%weapon-dexterity-scale :initform 3)))

(defclass sword (weapon)
  ((%damage :initform 25)
   (%weapon-cooldown :initform 4)
   (%weapon-strength-scale :initform 3)
   (%weapon-dexterity-scale :initform 2)))

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
   (%weapon-strength-scale :initform 2)
   (%weapon-dexterity-scale :initform 1)
   (%damage-reduction :initform 0.95)
   (%stability :initform 0.5)))
