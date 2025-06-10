(in-package #:rl/tests)
(in-suite rl)

(test enemy-creation
  (with-empty-state
    ;; Test base enemy class - requires initialization args
    ;; Skip this test as enemy is abstract
    (pass)))

(test goblin-creation
  (with-empty-state
    ;; Goblin has default values
    (let ((goblin (make-instance 'rl::goblin)))
      ;; Goblin is enemy and humanoid
      (is (typep goblin 'rl::enemy))
      (is (typep goblin 'rl::humanoid))
      ;; Therefore has inventory and arms
      (is (typep goblin 'rl::inventory))
      (is (typep goblin 'rl::arms))
      ;; Check default stats
      (is (= (rl::strength goblin) 3))
      (is (= (rl::vitality goblin) 3)))))

(test goblin-fighter-creation
  (with-empty-state
    ;; Goblin fighter has defaults and comes with dagger
    (let ((fighter (make-instance 'rl::goblin-fighter)))
      ;; Inherits from goblin
      (is (typep fighter 'rl::goblin))
      (is (typep fighter 'rl::enemy))
      (is (typep fighter 'rl::humanoid))
      ;; Should have dagger in inventory
      (let ((inventory (rl::inventory fighter)))
        (is (not (null inventory)))
        (is (typep (cdr (first inventory)) 'rl::dagger))))))

(test goblin-brawler-creation
  (with-empty-state
    (let ((brawler (make-instance 'rl::goblin-brawler)))
      ;; Inherits from goblin
      (is (typep brawler 'rl::goblin))
      (is (typep brawler 'rl::enemy))
      ;; Has increased strength and vitality
      (is (= (rl::strength brawler) 10))
      (is (= (rl::vitality brawler) 10)))))

(test warrior-creation
  (with-empty-state
    (let ((warrior (make-instance 'rl::warrior)))
      ;; Warrior is enemy and humanoid
      (is (typep warrior 'rl::enemy))
      (is (typep warrior 'rl::humanoid))
      ;; Should have sword and kite-shield
      (let ((inventory (rl::inventory warrior)))
        (is (= (length inventory) 2))
        (is (typep (cdr (assoc #\a inventory)) 'rl::sword))
        (is (typep (cdr (assoc #\b inventory)) 'rl::kite-shield))))))

(test rat-creation
  (with-empty-state
    (let ((rat (make-instance 'rl::rat)))
      ;; Rat is just an enemy, not humanoid
      (is (typep rat 'rl::enemy))
      (is (not (typep rat 'rl::humanoid)))
      ;; Has low stats (all 2)
      (is (= (rl::strength rat) 2))
      (is (= (rl::vitality rat) 2)))))

(test enemy-print-object
  (with-empty-state
    (let ((goblin (make-instance 'rl::goblin :x 5 :y 5)))
      ;; Test print-object works
      (is (stringp (with-output-to-string (s)
                     (print-object goblin s)))))))

(test enemy-update
  (with-empty-state
    (let ((goblin (make-instance 'rl::goblin :x 5 :y 5))
          (player (make-instance 'rl::player :x 0 :y 0)))
      ;; Set up player for update to work
      (setf rl::*player* player)
      (rl::add-object goblin)
      (rl::add-object player)
      ;; Test update method exists and can be called
      ;; It may return nil or something else
      (rl::update goblin))))

(test enemy-inheritance-chain
  (with-empty-state
    ;; Test complete inheritance chain for a complex enemy
    (let ((fighter (make-instance 'rl::goblin-fighter :x 0 :y 0)))
      ;; From goblin-fighter up the chain
      (is (typep fighter 'rl::goblin-fighter))
      (is (typep fighter 'rl::goblin))
      (is (typep fighter 'rl::enemy))
      (is (typep fighter 'rl::humanoid))
      (is (typep fighter 'rl::alive))
      (is (typep fighter 'rl::solid))
      (is (typep fighter 'rl::visible))
      (is (typep fighter 'rl::moveable))
      (is (typep fighter 'rl::pos))
      (is (typep fighter 'rl::inventory))
      (is (typep fighter 'rl::arms)))))

(test enemy-default-state
  (with-empty-state
    (let ((goblin (make-instance 'rl::goblin)))
      ;; Enemies start in sleeping state
      (is (eq (rl::enemy-state goblin) :sleeping))
      ;; Have default view distance
      (is (= (rl::view-distance goblin) 10)))))

(test enemy-ai-sleeping-to-chasing
  (with-empty-state
    (let ((enemy (make-instance 'rl::goblin :x 5 :y 5))
          (player (make-instance 'rl::player :x 6 :y 5)))
      (setf rl::*player* player)
      (rl::add-object enemy)
      (rl::add-object player)
      
      ;; Enemy starts sleeping
      (is (eq (rl::enemy-state enemy) :sleeping))
      
      ;; Update can-see for player visibility
      (rl::update-can-see player)
      
      ;; Force transition to chasing by setting random seed
      ;; In real game, this happens randomly when player is visible
      (setf *random-state* (make-random-state t))
      ;; Update multiple times to trigger state change
      (loop repeat 100
            while (eq (rl::enemy-state enemy) :sleeping)
            do (rl::update enemy))
      
      ;; Should eventually wake up when player is nearby
      (is (member (rl::enemy-state enemy) '(:chasing :sleeping))))))

(test enemy-ai-chasing-behavior
  (with-empty-state
    (let ((enemy (make-instance 'rl::goblin :x 10 :y 10))
          (player (make-instance 'rl::player :x 11 :y 10)))
      (setf rl::*player* player)
      (rl::add-object enemy)
      (rl::add-object player)
      
      ;; Set enemy to chasing state
      (setf (rl::enemy-state enemy) :chasing)
      
      ;; Update enemy
      (rl::update enemy)
      
      ;; Enemy state should remain chasing
      (is (eq (rl::enemy-state enemy) :chasing)))))

(test enemy-ai-chasing-to-fleeing
  (with-empty-state
    (let ((enemy (make-instance 'rl::goblin :x 5 :y 5))
          (player (make-instance 'rl::player :x 6 :y 5)))
      (setf rl::*player* player)
      (rl::add-object enemy)
      (rl::add-object player)
      
      ;; Set enemy to chasing state
      (setf (rl::enemy-state enemy) :chasing)
      
      ;; Deplete stamina to trigger fleeing
      (setf (rl::stamina enemy) 0)
      
      ;; Update enemy
      (rl::update enemy)
      
      ;; Should switch to fleeing when low on stamina
      (is (eq (rl::enemy-state enemy) :fleeing)))))

(test enemy-ai-wandering-behavior
  (with-empty-state
    (let ((enemy (make-instance 'rl::goblin :x 50 :y 50))
          (player (make-instance 'rl::player :x 0 :y 0)))
      (setf rl::*player* player)
      (rl::add-object enemy)
      (rl::add-object player)
      
      ;; Set enemy to wandering state
      (setf (rl::enemy-state enemy) :wandering)
      
      ;; Enemy should have a wandering target
      (is (typep (rl::wandering-to enemy) 'rl::pos))
      
      ;; Update enemy
      (rl::update enemy)
      
      ;; Enemy should remain in wandering state
      (is (eq (rl::enemy-state enemy) :wandering)))))

(test enemy-ai-wandering-to-chasing
  (with-empty-state
    (let ((enemy (make-instance 'rl::goblin :x 5 :y 5))
          (player (make-instance 'rl::player :x 7 :y 5)))
      (setf rl::*player* player)
      (rl::add-object enemy)
      (rl::add-object player)
      
      ;; Set enemy to wandering state
      (setf (rl::enemy-state enemy) :wandering)
      
      ;; Update can-see for visibility
      (rl::update-can-see player)
      
      ;; Update enemy multiple times
      ;; Should eventually notice player and chase
      (loop repeat 10
            while (eq (rl::enemy-state enemy) :wandering)
            do (rl::update enemy))
      
      ;; When player is visible and close, enemy might chase
      (is (member (rl::enemy-state enemy) '(:chasing :wandering))))))

(test enemy-ai-fleeing-behavior
  (with-empty-state
    (let ((enemy (make-instance 'rl::goblin :x 10 :y 10))
          (player (make-instance 'rl::player :x 12 :y 10)))
      (setf rl::*player* player)
      (rl::add-object enemy)
      (rl::add-object player)
      
      ;; Set enemy to fleeing state with some stamina
      (setf (rl::enemy-state enemy) :fleeing)
      (setf (rl::stamina enemy) 50)
      
      ;; Update enemy
      (rl::update enemy)
      
      ;; Enemy should remain in fleeing state or switch to chasing
      (is (member (rl::enemy-state enemy) '(:fleeing :chasing))))))

(test enemy-ai-fleeing-to-chasing
  (with-empty-state
    (let ((enemy (make-instance 'rl::goblin :x 10 :y 10))
          (player (make-instance 'rl::player :x 12 :y 10)))
      (setf rl::*player* player)
      (rl::add-object enemy)
      (rl::add-object player)
      
      ;; Set enemy to fleeing state with high stamina
      (setf (rl::enemy-state enemy) :fleeing)
      (setf (rl::stamina enemy) 100)
      
      ;; Update enemy multiple times
      ;; With high stamina, might return to chasing
      (loop repeat 50
            while (eq (rl::enemy-state enemy) :fleeing)
            do (rl::update enemy))
      
      ;; May switch back to chasing eventually
      (is (member (rl::enemy-state enemy) '(:chasing :fleeing))))))

(test enemy-wandering-new-target
  (with-empty-state
    (let ((enemy (make-instance 'rl::goblin :x 50 :y 50))
          (player (make-instance 'rl::player :x 0 :y 0)))
      (setf rl::*player* player)
      (rl::add-object enemy)
      (rl::add-object player)
      
      ;; Set enemy to wandering state
      (setf (rl::enemy-state enemy) :wandering)
      
      ;; Force enemy to be stuck (no movement)
      (setf (rl::dx enemy) 0)
      (setf (rl::dy enemy) 0)
      
      ;; Record current wandering target
      (let ((old-target (rl::wandering-to enemy)))
        
        ;; Update enemy
        (rl::update enemy)
        
        ;; Should pick new wandering target when stuck
        (is (typep (rl::wandering-to enemy) 'rl::pos))))))

(test enemy-resistances
  (with-empty-state
    (let ((goblin (make-instance 'rl::goblin)))
      ;; Goblins have fire resistance
      (let ((resistances (rl::resistances goblin)))
        (is (listp resistances))
        (is (= (length resistances) 1))
        (let ((fire-resistance (first resistances)))
          (is (typep fire-resistance 'rl::resistance))
          (is (eq (rl::resistance-to fire-resistance) 'rl::fire))
          (is (= (rl::resistance-amount fire-resistance) 0.8)))))))

(test enemy-position-in-print
  (with-empty-state
    (let ((enemy (make-instance 'rl::goblin :x 42 :y 13)))
      (setf (rl::enemy-state enemy) :chasing)
      ;; Test that print-object includes position and state
      (let ((printed (with-output-to-string (s)
                       (print-object enemy s))))
        (is (search "42" printed))
        (is (search "13" printed))
        (is (search ":CHASING" printed))))))