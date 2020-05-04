(in-package #:rl)

(defclass weapon (item)
  ((%weapon-cooldown :initarg :weapon-cooldown :initform 4 :accessor weapon-cooldown)))

(defclass sword (weapon damage)
  ())
