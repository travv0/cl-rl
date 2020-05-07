(in-package #:rl)

(defmethod collide ((obj pos) (moving-obj moveable)))

(defmethod collide :before ((door door) (moving-obj cooldown))
  (when (typep door 'solid)
    (incf (cooldown moving-obj) 3)
    (delete-from-mix door 'opaque 'solid)))

(defmethod collide :before ((obj item) (moving-obj inventory))
  (write-to-log "picked up ~:[something~;a~@[n~] ~0@*~a~]"
                (display-name obj)
                (member (char (display-name obj) 0) '(#\a #\e #\i #\o #\u)))
  (if (typep obj 'weapon)
      (setf (equip-right-arm moving-obj) obj)
      (push obj (inventory moving-obj)))
  (ensure-mix obj 'deleted))

(defparameter *unarmed-damage* 2)
(defparameter *unarmed-stamina* 10)
(defparameter *unarmed-cooldown* 2)
(defparameter *unarmed-windup* 1)

(defmethod collide :before ((obj health) (arm right-arm))
  (write-to-log "~a ~:[is preparing~;raised their weapon~] to attack"
                (display-name arm)
                (equip-right-arm arm))
  (ensure-mix arm 'attacking)
  (setf (current-windup arm) (if (equip-right-arm arm)
                                 (weapon-windup (equip-right-arm arm))
                                 *unarmed-windup*))
  (setf (attacking-pos arm) (pos (x obj) (y obj))))

(defmethod attack :after (obj (arm attacking))
  (delete-from-mix arm 'attacking))

(defmethod attack (obj (arm right-arm))
  (write-to-log "~a hit only air!" (display-name arm)))

(defmethod attack ((obj health) (arm right-arm))
  (let ((damage (calculate-damage (equip-right-arm arm) (resistances obj)))
        (stamina-use (if (equip-right-arm arm)
                         (stamina-use (equip-right-arm arm))
                         *unarmed-stamina*)))
    (cond ((>= (stamina arm) stamina-use)
           (write-to-log "~a attacked ~a for ~d damage"
                         (display-name arm)
                         (display-name obj)
                         damage)
           (when (typep arm 'cooldown)
             (incf (cooldown arm) (if (equip-right-arm arm)
                                      (weapon-cooldown (equip-right-arm arm))
                                      *unarmed-cooldown*)))
           (decf (health obj) damage)
           (decf (stamina arm) stamina-use)
           (when (not (plusp (health obj)))
             (write-to-log "~a was defeated" (display-name obj))
             (ensure-mix obj 'deleted)))
          (t (write-to-log "~a could not attack - insufficient stamina"
                           (display-name arm))))))

(defmethod calculate-damage (weapon &optional resistances)
  (declare (ignore resistances))
  (ceiling (* (+ 0.8 (random 0.4)) *unarmed-damage*)))

(defmethod calculate-damage ((weapon weapon) &optional resistances)
  (let ((damage (damage weapon)))
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
