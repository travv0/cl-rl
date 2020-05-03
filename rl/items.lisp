(in-package #:rl)

(defclass item (visible)
  ((%char :initform #\?)))

(defclass potion (item)
  ((%char :initform #\!)
   (%foreground-color :initform :yellow)))
