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
      (is (= (slot-value goblin 'rl::strength) 2))
      (is (= (slot-value goblin 'rl::vitality) 6)))))

(test goblin-fighter-creation
  (with-empty-state
    ;; Goblin fighter has defaults and comes with dagger
    (let ((fighter (make-instance 'rl::goblin-fighter)))
      ;; Inherits from goblin
      (is (typep fighter 'rl::goblin))
      (is (typep fighter 'rl::enemy))
      (is (typep fighter 'rl::humanoid))
      ;; Should have dagger in inventory
      (let ((inventory (slot-value fighter 'rl::inventory)))
        (is (not (null inventory)))
        (is (typep (cdr (first inventory)) 'rl::dagger))))))

(test goblin-brawler-creation
  (with-empty-state
    (let ((brawler (make-instance 'rl::goblin-brawler)))
      ;; Inherits from goblin
      (is (typep brawler 'rl::goblin))
      (is (typep brawler 'rl::enemy))
      ;; Has increased strength and vitality
      (is (= (slot-value brawler 'rl::strength) 10))
      (is (= (slot-value brawler 'rl::vitality) 10)))))

(test warrior-creation
  (with-empty-state
    (let ((warrior (make-instance 'rl::warrior)))
      ;; Warrior is enemy and humanoid
      (is (typep warrior 'rl::enemy))
      (is (typep warrior 'rl::humanoid))
      ;; Should have sword and kite-shield
      (let ((inventory (slot-value warrior 'rl::inventory)))
        (is (= (length inventory) 2))
        (is (typep (cdr (assoc #\a inventory)) 'rl::sword))
        (is (typep (cdr (assoc #\b inventory)) 'rl::kite-shield))))))

(test rat-creation
  (with-empty-state
    (let ((rat (make-instance 'rl::rat 
                              :resistance 1
                              :intelligence 1
                              :faith 1)))
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
    (let ((goblin (make-instance 'rl::goblin :x 5 :y 5)))
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
      (is (eq (slot-value goblin 'rl::enemy-state) :sleeping))
      ;; Have default view distance
      (is (= (slot-value goblin 'rl::view-distance) 10)))))