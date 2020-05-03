(in-package #:rl)

(defclass weapon (item)
  ((%char :initform #\))
   (%weapon-cooldown :initarg :weapon-cooldown :initform 4 :accessor weapon-cooldown)))

(defclass sword (weapon damage)
  ((%foreground-color :initform :magenta)))
