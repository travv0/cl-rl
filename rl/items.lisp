(in-package #:rl)

(defclass item (visible)
  ())

(defclass rechargeable ()
  ((%max-charges :initarg :charges
                 :initform (error "max-charges is required")
                 :accessor max-charges)
   (%current-charges :accessor current-charges)))

(defmethod initialize-instance :after ((obj rechargeable) &key)
  (setf (current-charges obj) (max-charges obj)))

(defclass health-potion (rechargeable item)
  ((%regeneration-amount :initform 40 :reader regeneration-amount)
   (%max-charges :initform 5)))

(defmethod apply-item :before ((item rechargeable) obj)
  (unless (plusp (current-charges item))
    (return-from apply-item)))

(defmethod apply-item ((potion health-potion) (obj alive))
  (incf (health obj) (regeneration-amount potion)))

(defmethod apply-item :after ((item rechargeable) obj)
  (decf (current-charges item)))
