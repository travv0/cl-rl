(in-package #:rl)

(defclass enemy (solid moveable visible health stamina)
  ((%enemy-state :initarg :enemy-state
                 :initform (if (zerop (random 2))
                               :wandering
                               :sleeping)
                 :accessor enemy-state)
   (%wandering-to :initform (random-pos)
                  :accessor wandering-to)
   (%view-distance :initform 30 :initarg :view-distance :accessor view-distance)))

(defclass goblin (enemy right-arm)
  ((%health :initform 40)
   (%stamina :initform 40)
   (%resistances :initform (list (make-resistance 'fire 0.8)))))

(defclass goblin-fighter (goblin)
  ((%equip-right-arm :initform (make-instance 'dagger))))

(defclass warrior (enemy right-arm)
  ((%equip-right-arm :initform (make-instance 'sword))))

(defclass rat (enemy right-arm)
  ((%health :initform 20)))

(defmethod print-object ((enemy enemy) stream)
  (print-unreadable-object (enemy stream :type t :identity t)
    (format stream "(~d, ~d) ~s" (x enemy) (y enemy) (enemy-state enemy))))

(defmethod update :before ((enemy enemy))
  (ccase (enemy-state enemy)
    (:chasing (if (and (zerop (random 10))
                       (not (can-see-p enemy *player*)))
                  (setf (enemy-state enemy) :wandering)
                  (move-toward-goal enemy *player*)))
    (:wandering
     (move-toward-goal enemy (wandering-to enemy))
     (cond ((and (< (random 5) 4)
                 (< (distance enemy *player*) (view-distance enemy))
                 (can-see-p enemy *player*))
            (setf (enemy-state enemy) :chasing))
           ((and (zerop (dx enemy)) (zerop (dy enemy)))
            (setf (wandering-to enemy) (random-pos)))))
    (:sleeping (when (and (zerop (random 5))
                          (< (distance enemy *player*) (view-distance enemy))
                          (can-see-p enemy *player*))
                 (setf (enemy-state enemy) :chasing)))))
