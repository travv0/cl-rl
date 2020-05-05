(in-package #:rl)

(defclass weapon (item damage stamina-use)
  ((%weapon-cooldown :initarg :weapon-cooldown
                     :initform (error "weapon-cooldown must be initialized")
                     :accessor weapon-cooldown)))

(defclass dagger (weapon)
  ((%damage :initform 15)
   (%weapon-cooldown :initform 2)))

(defclass sword (weapon)
  ((%damage :initform 25)
   (%weapon-cooldown :initform 4)))

(defmethod initialize-instance :after ((weapon weapon) &key)
  (unless (slot-boundp weapon '%stamina-use)
    (setf (stamina-use weapon) (+ (damage weapon) 5))))
