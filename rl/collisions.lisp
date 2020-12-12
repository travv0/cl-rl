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

(defparameter *unarmed-damage* 5)
(defparameter *unarmed-stamina* 10)
(defparameter *unarmed-cooldown* 2)
(defparameter *unarmed-windup* 1)

(defmethod stamina-use ((arm arm))
  (if (equipped-weapon arm)
      (stamina-use (equipped-weapon arm))
      *unarmed-stamina*))

(defmethod attack-stamina-use ((obj arms))
  (reduce (lambda (acc arm)
            (+ acc
               (if (equipped-weapon arm)
                   (stamina-use arm)
                   *unarmed-stamina*)))
          (arms obj)
          :initial-value 0))

(defmethod weapon-windup ((arm arm))
  (if (equipped-weapon arm)
      (weapon-windup (equipped-weapon arm))
      *unarmed-windup*))

(defmethod weapon-cooldown ((arm arm))
  (if (equipped-weapon arm)
      (weapon-cooldown (equipped-weapon arm))
      *unarmed-cooldown*))

(defmethod collide :before ((obj alive) (attacker arms))
  (let ((stamina-use (attack-stamina-use attacker)))
    (when (or (typep attacker 'player)
              (plusp (- (stamina attacker) stamina-use)))
      (if (>= (stamina attacker) stamina-use)
          (attack obj attacker)
          (write-to-log "~a could not attack - insufficient stamina"
                        (display-name attacker))))))

(defmethod attack :after (obj (attacker arms))
  (dolist (arm (arms attacker))
    (let ((stamina-use (stamina-use arm)))
      (when (typep arm 'cooldown)
        (let ((cooldown (weapon-cooldown arm)))
          (incf (cooldown attacker) cooldown)))
      (decf (stamina attacker) stamina-use))))

(defmethod attack ((obj alive) (attacker arms))
  (let* ((damage (calculate-damage attacker (resistances obj))))
    (write-to-log "~a attacked ~a for ~d damage"
                  (display-name attacker)
                  (display-name obj)
                  damage)
    (decf (health obj) damage)
    (when (not (plusp (health obj)))
      (write-to-log "~a was defeated" (display-name obj))
      (ensure-mix obj 'deleted))))

(defun damage-mod (damage)
  (ceiling (* (+ 0.25 (random 1.0)) damage)))

(defmethod calculate-damage ((attacker alive) &optional resistances)
  (if (typep attacker 'arms)
      (loop for arm in (arms attacker)
            summing
            (if-let ((weapon (equipped-weapon arm)))
              (let ((damage (* (1+ (* (/ (strength attacker) 50) (weapon-strength-scale weapon)))
                               (1+ (* (/ (dexterity attacker) 50) (weapon-dexterity-scale weapon)))
                               (damage weapon))))
                (dolist (modifier (get-modifiers weapon))
                  (let ((modifier-damage 1.2))
                    (when-let ((resistance (find (class-name modifier) resistances :key 'resistance-to :test #'string=)))
                      (setf modifier-damage (- (* 2 modifier-damage)
                                               (* (resistance-amount resistance) modifier-damage))))
                    (setf damage (* damage 0.95 modifier-damage))))
                (damage-mod damage))
              (damage-mod *unarmed-damage*)))
      0))

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
