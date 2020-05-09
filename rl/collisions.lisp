(in-package #:rl)

(defmethod collide ((obj pos) (moving-obj moveable)))

(defmethod collide ((door door) (moving-obj cooldown))
  (when (typep door 'solid)
    (incf (cooldown moving-obj) 3)
    (delete-from-mix door 'opaque 'solid)))

(defmethod collide ((obj item) (moving-obj inventory))
  (write-to-log "~a picked up ~:[something~;a~@[n~] ~1@*~a~]"
                (display-name moving-obj)
                (display-name obj)
                (member (char (display-name obj) 0) '(#\a #\e #\i #\o #\u)))
  (if (typep obj 'weapon)
      (setf (equip-right-arm moving-obj) obj)
      (push obj (inventory moving-obj)))
  (ensure-mix obj 'deleted))

(defparameter *unarmed-damage* 10)
(defparameter *unarmed-stamina* 10)
(defparameter *unarmed-cooldown* 2)
(defparameter *unarmed-windup* 1)

(defmethod collide :before ((obj alive) (arm right-arm))
  (cond ((typep arm 'blocking) (write-to-log "~a could not attack - shield is raised"
                                             (display-name arm)))
        ((>= (stamina arm) (if (equip-right-arm arm)
                               (stamina-use (equip-right-arm arm))
                               *unarmed-stamina*))
         (write-to-log "~a ~:[is preparing~;raised their weapon~] to attack"
                       (display-name arm)
                       (equip-right-arm arm))
         (ensure-mix arm 'attacking)
         (setf (current-windup arm) (if (equip-right-arm arm)
                                        (weapon-windup (equip-right-arm arm))
                                        *unarmed-windup*))
         (setf (attacking-pos arm) (pos (x obj) (y obj))))
        (t (write-to-log "~a could not attack - insufficient stamina"
                         (display-name arm)))))

(defmethod attack :after (obj (arm attacking))
  (delete-from-mix arm 'attacking))

(defmethod attack :after (obj (arm right-arm))
  (let ((stamina-use (if (equip-right-arm arm)
                         (stamina-use (equip-right-arm arm))
                         *unarmed-stamina*)))
    (when (typep arm 'cooldown)
      (let ((cooldown (if (equip-right-arm arm)
                          (weapon-cooldown (equip-right-arm arm))
                          *unarmed-cooldown*)))
        (cond ((typep obj 'blocking)
               (incf (cooldown arm) (* 2 cooldown))
               (write-to-log "~a's attacked was deflected by ~a's shield"
                             (display-name arm) (display-name obj)))
              (t (incf (cooldown arm) cooldown)))))
    (decf (stamina arm) stamina-use)))

(defmethod attack (obj (arm right-arm))
  (write-to-log "~a hit only air!" (display-name arm)))

(defmethod attack ((obj alive) (arm right-arm))
  (let* ((raw-damage (calculate-damage (equip-right-arm arm) arm (resistances obj)))
         (damage (if (typep obj 'blocking)
                     (ceiling (- raw-damage (* raw-damage (damage-reduction (equip-left-arm obj)))))
                     raw-damage))
         (stamina-use (if (equip-right-arm arm)
                          (stamina-use (equip-right-arm arm))
                          *unarmed-stamina*)))
    (when (typep obj 'blocking)
      (decf (stamina obj)
            (ceiling (* 2 (- stamina-use
                             (* stamina-use (stability (equip-left-arm obj)))))))
      (when (<= (stamina obj) 0)
        (setf (stamina obj) 0
              damage raw-damage)
        (delete-from-mix obj 'blocking)
        (write-to-log "~a was too exhausted to block ~a's attack and lowered their shield"
                      (display-name obj) (display-name arm))))

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
                when (and (not (typep other-obj 'cell))
                          (not (typep other-obj 'memory))
                          (same step other-obj))
                  do (push (cons other-obj previous-step) collisions)
                do (setf previous-step step))
        finally (return collisions)))
