(in-package #:rl)

(defclass enemy (solid moveable visible health)
  ((%enemy-state :initarg :enemy-state
                 :initform (if (zerop (random 2))
                               :wandering
                               :sleeping)
                 :accessor enemy-state)
   (%wandering-to :initform (random-pos)
                  :accessor wandering-to)
   (%char :initform (error "enemies must have a char set"))))

(defclass goblin (enemy right-arm)
  ((%char :initform #\g)
   (%foreground-color :initform :green)
   (%bold-color :initform nil)
   (%resistances :initform (list (make-resistance 'fire 0.8)))))

(defclass goblin-fighter (goblin)
  ((%bold-color :initform t)
   (%equip-right-arm :initform (make-instance 'sword))))

(defmethod print-object ((enemy enemy) stream)
  (print-unreadable-object (enemy stream :type t :identity t)
    (format stream "(~d, ~d) ~s" (x enemy) (y enemy) (enemy-state enemy))))

(defmethod update :before ((enemy enemy))
  (ccase (enemy-state enemy)
    (:chasing (if (and (not (can-see-p enemy *player*))
                       (zerop (random 10)))
                  (setf (enemy-state enemy) :wandering)
                  (move-toward-goal enemy *player*)))
    (:wandering
     (move-toward-goal enemy (wandering-to enemy))
     (cond ((and (can-see-p enemy *player*)
                 (< (random 5) 4))
            (setf (enemy-state enemy) :chasing))
           ((and (zerop (dx enemy)) (zerop (dy enemy)))
            (setf (wandering-to enemy) (random-pos)))))
    (:sleeping (when (and (can-see-p enemy *player*)
                          (zerop (random 5)))
                 (setf (enemy-state enemy) :chasing)))))
