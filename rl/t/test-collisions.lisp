(in-package #:rl/tests)
(in-suite rl)

(test damage-mod-function
  (with-empty-state
    ;; damage-mod applies randomization
    (let ((base-damage 10))
      (dotimes (i 10)
        (let ((modified (rl::damage-mod base-damage)))
          ;; Should be within reasonable range
          (is (numberp modified))
          (is (> modified 0)))))))

(test collide-base-method
  (with-empty-state
    ;; Base collide method exists - takes pos and moveable
    (let ((obj1 (make-instance 'rl::pos))
          (obj2 (make-instance 'rl::player))) ;; player is moveable
      ;; Should be callable (returns nil by default)
      (is (null (rl::collide obj1 obj2))))))

(test pick-up-method
  (with-empty-state
    (let ((player (make-instance 'rl::player))
          (item (make-instance 'rl::item :x 0 :y 0)))
      ;; pick-up should be callable - takes item first, then inventory object
      (rl::pick-up item player))))

(test attack-method
  (with-empty-state
    ;; attack method needs alive and armed objects
    (let ((attacker (make-instance 'rl::player))
          (target (make-instance 'rl::goblin)))
      ;; Should be callable
      (rl::attack attacker target))))

(test stamina-use-method
  (with-empty-state
    (let ((arm (make-instance 'rl::arm)))
      ;; Should return a number
      (is (numberp (rl::stamina-use arm))))))

(test weapon-cooldown-method
  (with-empty-state
    (let ((arm (make-instance 'rl::arm)))
      ;; Should return a number
      (is (numberp (rl::weapon-cooldown arm))))))

(test weapon-windup-method
  (with-empty-state
    (let ((arm (make-instance 'rl::arm)))
      ;; Should return a number  
      (is (numberp (rl::weapon-windup arm))))))

(test calculate-damage-method
  (with-empty-state
    ;; Test with proper objects - calculate-damage expects an alive attacker
    (let ((attacker (make-instance 'rl::player))
          (target (make-instance 'rl::goblin)))
      ;; Should return a number
      (is (numberp (rl::calculate-damage attacker (rl::resistances target)))))))

(test check-collisions-method
  (with-empty-state
    ;; Add some terrain
    (rl::add-object (rl::make-grass 5 5))
    (rl::add-object (rl::make-wall 6 5))
    
    (let ((obj (make-instance 'rl::player :x 5 :y 5)))
      (rl::add-object obj)
      ;; Set dx to move east
      (setf (rl::dx obj) 1)
      ;; Test collision checking along a path
      (let ((result (rl::check-collisions obj)))
        ;; Should return a list (possibly empty)
        (is (listp result))))))

(test door-collision
  (with-empty-state
    (let ((door (rl::make-door 0 0)))
      ;; Door starts as solid and opaque
      (is (typep door 'rl::opaque))
      (is (typep door 'rl::solid))
      
      ;; Collide with player to open
      (rl::collide door (make-instance 'rl::player))
      
      ;; Door should no longer be solid/opaque
      (is (not (typep door 'rl::opaque)))
      (is (not (typep door 'rl::solid))))))

(test attack-stamina-use
  (with-empty-state
    (let ((attacker (make-instance 'rl::player)))
      ;; Should return a number
      (is (numberp (rl::attack-stamina-use attacker))))))

(test item-pickup-collision
  (with-empty-state
    (let ((player (make-instance 'rl::player))
          (item (make-instance 'rl::item :x 0 :y 0)))
      ;; Item should not be deleted initially
      (is (not (typep item 'rl::deleted)))
      
      ;; Collide to pick up
      (rl::collide item player)
      
      ;; Item should be picked up (marked as deleted)
      (is (typep item 'rl::deleted)))))

(test wall-collision
  (with-empty-state
    (let ((wall (make-instance 'rl::wall))
          (player (make-instance 'rl::player)))
      ;; Colliding with a wall should do nothing special
      ;; Just verify it doesn't error
      (rl::collide wall player))))

(test enemy-collision
  (with-empty-state
    (let ((player (make-instance 'rl::player))
          (goblin (make-instance 'rl::goblin)))
      ;; Store initial health
      (let ((player-health (rl::health player))
            (goblin-health (rl::health goblin)))
        ;; Collide enemy with player (enemy attacks)
        (rl::collide player goblin)
        ;; Player should take damage
        (is (< (rl::health player) player-health))
        ;; Goblin should not take damage
        (is (= (rl::health goblin) goblin-health))))))

(test weapon-collision
  (with-empty-state
    (let ((player (make-instance 'rl::player))
          (goblin (make-instance 'rl::goblin)))
      ;; Give player a weapon
      (let ((sword (make-instance 'rl::sword)))
        (rl::add-to-inventory sword player)
        (setf (rl::equipped-weapon (first (rl::arms player))) sword))
      
      ;; Store initial health
      (let ((goblin-health (rl::health goblin)))
        ;; Player attacks goblin
        (rl::collide goblin player)
        ;; Goblin should take damage
        (is (< (rl::health goblin) goblin-health))))))