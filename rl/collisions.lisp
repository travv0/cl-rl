(in-package #:rl)

(defmethod collide ((obj pos) (moving-obj moveable)))

(defmethod collide ((door door) (moving-obj cooldown))
  (when (typep door 'solid)
    (incf (cooldown moving-obj) 3)
    (delete-from-mix door 'opaque 'solid)))

(defmethod pick-up :before ((obj item) (moving-obj inventory))
  (write-to-log "~a picked up ~:[something~;a~@[n~] ~1@*~a~]"
                (display-name moving-obj)
                (display-name obj)
                (member (char (display-name obj) 0) '(#\a #\e #\i #\o #\u))))

(defmethod pick-up ((obj item) (moving-obj inventory))
  (add-to-inventory obj moving-obj))

(defmethod collide ((obj item) (moving-obj inventory))
  (if (pick-up obj moving-obj)
      (ensure-mix obj 'deleted)
      (write-to-log "~a was unable to pick up ~a - inventory full"
                    (display-name moving-obj)
                    (display-name obj))))

(defparameter *unarmed-damage* 10)
(defparameter *unarmed-stamina* 10)
(defparameter *unarmed-cooldown* 2)
(defparameter *unarmed-windup* 1)

(defmethod stamina-use ((arm right-arm))
  (if (equip-right-arm arm)
      (stamina-use (equip-right-arm arm))
      *unarmed-stamina*))

(defmethod weapon-windup ((arm right-arm))
  (if (equip-right-arm arm)
      (weapon-windup (equip-right-arm arm))
      *unarmed-windup*))

(defmethod weapon-cooldown ((arm right-arm))
  (if (equip-right-arm arm)
      (weapon-cooldown (equip-right-arm arm))
      *unarmed-cooldown*))

(defmethod collide :before ((obj alive) (arm right-arm))
  (let ((stamina-use (stamina-use arm)))
    (when (or (typep arm 'player)
              (and (plusp (- (stamina arm) stamina-use))
                   (zerop (random (* 2 (floor (max-stamina arm) (- (stamina arm) stamina-use)))))))
      (if (>= (stamina arm) stamina-use)
          (attack obj arm)
          (write-to-log "~a could not attack - insufficient stamina"
                        (display-name arm))))))

(defmethod attack :after (obj (arm right-arm))
  (let ((stamina-use (stamina-use arm)))
    (when (typep arm 'cooldown)
      (let ((cooldown (weapon-cooldown arm)))
        (incf (cooldown arm) cooldown)))
    (decf (stamina arm) stamina-use)))

(defmethod attack ((obj alive) (arm right-arm))
  (let* ((damage (calculate-damage (equip-right-arm arm) arm (resistances obj))))
    (write-to-log "~a attacked ~a for ~d damage"
                  (display-name arm)
                  (display-name obj)
                  damage)
    (decf (health obj) damage)
    (when (not (plusp (health obj)))
      (write-to-log "~a was defeated" (display-name obj))
      (ensure-mix obj 'deleted))))

(defmethod calculate-damage (weapon strength-stat &optional resistances)
  (declare (ignore resistances))
  (ceiling (* (+ 0.8 (random 0.4)) *unarmed-damage*)))

(defmethod calculate-damage ((weapon weapon) (attacker alive) &optional resistances)
  (let ((damage (* (1+ (* (/ (strength attacker) 50) (weapon-strength-scale weapon)))
                   (1+ (* (/ (dexterity attacker) 50) (weapon-dexterity-scale weapon)))
                   (damage weapon))))
    (dolist (modifier (get-modifiers weapon))
      (let ((modifier-damage 1.2))
        (when-let ((resistance (find modifier resistances :key 'resistance-to)))
          (setf modifier-damage (- (* 2 modifier-damage)
                                   (* (resistance-amount resistance) modifier-damage))))
        (setf damage (* damage 0.95 modifier-damage))))
    (ceiling (* (+ 0.8 (random 0.4)) damage))))

(defmethod check-collisions ((obj moveable))
  (declare (optimize speed))
  (loop with collisions = '()
        with previous-step = obj
        for step in (rest (get-line obj (pos (+ (x obj) (dx obj))
                                             (+ (y obj) (dy obj)))))
        for objs = (get-objects-at-pos step) do
          (loop for other-obj in objs
                when (same step other-obj)
                  do (push (cons other-obj previous-step) collisions)
                do (setf previous-step step))
        finally (return collisions)))
