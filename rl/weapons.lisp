(in-package #:rl)

(defclass weapon (item damage stamina-use)
  ((%weapon-cooldown :initarg :weapon-cooldown :initform 4 :accessor weapon-cooldown)))

(defclass sword (weapon)
  ((%damage :initform 15)
   (%stamina-use :initform 20)))
