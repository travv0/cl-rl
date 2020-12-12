(in-package #:rl/tests)
(in-suite rl)

(test opening-door
  (with-empty-state
    (let ((door (rl::make-door 0 0)))
      (is (and (typep door 'rl::opaque)
               (typep door 'rl::solid)))
      (rl::collide door (make-instance 'rl::player))
      (is (and (not (typep door 'rl::opaque))
               (not (typep door 'rl::solid)))))))

(test picking-up-item
  (with-empty-state
    (let ((player (make-instance 'rl::player :inventory '()))
          (item (make-instance 'rl::sword)))
      (is (null (rl::inventory player)))
      (is (not (typep item 'rl::deleted)))
      (rl::collide item player)
      (is (eq (assoc-value (rl::inventory player) #\a) item))
      (is (typep item 'rl::deleted)))))

(test unarmed-attacking
  (with-empty-state
    (let* ((attacker (make-instance 'rl::goblin))
           (attackee (make-instance 'rl::player))
           (attacker-health (rl::health attacker))
           (attacker-stamina (rl::stamina attacker))
           (attackee-health (rl::health attackee))
           (attackee-stamina (rl::stamina attackee)))
      (rl::collide attackee attacker)
      (is (= (rl::health attacker) attacker-health))
      (is (< (rl::stamina attacker) attacker-stamina))
      (is (< (rl::health attackee) attackee-health))
      (is (= (rl::stamina attackee) attackee-stamina)))))

(test armed-attacking
  (with-empty-state
    (let* ((attacker (make-instance 'rl::goblin-fighter))
           (attackee (make-instance 'rl::player))
           (attacker-health (rl::health attacker))
           (attacker-stamina (rl::stamina attacker))
           (attackee-health (rl::health attackee))
           (attackee-stamina (rl::stamina attackee)))
      (rl::collide attackee attacker)
      (is (= (rl::health attacker) attacker-health))
      (is (< (rl::stamina attacker) attacker-stamina))
      (is (< (rl::health attackee) attackee-health))
      (is (= (rl::stamina attackee) attackee-stamina)))))
