(in-package #:rl/tests)
(in-suite rl)

(test cooldown-functionality
  (with-empty-state
    (let ((obj (make-instance 'rl::cooldown)))
      ;; Use the correct slot name and accessor
      (is (= (rl::cooldown obj) 0))
      (setf (rl::cooldown obj) 5)
      (is (rl::cooling-down-p obj))
      (setf (rl::cooldown obj) 0)
      (is (not (rl::cooling-down-p obj))))))

(test modifier-system
  (with-empty-state
    ;; Test get-modifiers function
    (let ((obj (make-instance 'rl::fire)))
      ;; Fire is a modifier itself
      (is (typep obj 'rl::modifier)))))

(test resistance-creation
  (with-empty-state
    ;; make-resistance requires resistance-to argument
    (let ((resist (rl::make-resistance 'physical)))
      (is (typep resist 'rl::resistance))
      (is (eq (rl::resistance-to resist) 'physical)))))

(test deleted-mixin-functionality
  (with-empty-state
    ;; Deleted is a simple marker class
    (let ((obj (make-instance 'rl::deleted)))
      (is (typep obj 'rl::deleted)))))

(test visibility-mixin
  (with-empty-state
    (let ((obj (make-instance 'rl::visible :x 5 :y 5)))
      ;; Test it inherits from pos
      (is (= (rl::x obj) 5))
      (is (= (rl::y obj) 5)))))

(test solid-mixin
  (with-empty-state
    (let ((obj (make-instance 'rl::solid)))
      (is (typep obj 'rl::solid)))))

(test opaque-mixin
  (with-empty-state
    (let ((obj (make-instance 'rl::opaque)))
      (is (typep obj 'rl::opaque)))))

(test inventory-mixin
  (with-empty-state
    (let ((obj (make-instance 'rl::inventory)))
      (is (typep obj 'rl::inventory))
      ;; Inventory slot is initialized to empty list
      (is (listp (rl::inventory obj)))
      (is (null (rl::inventory obj))))))

(test arm-class
  (with-empty-state
    (let ((arm (make-instance 'rl::arm)))
      (is (typep arm 'rl::arm))
      ;; Test print-object works
      (is (stringp (with-output-to-string (s)
                     (print-object arm s)))))))

(test humanoid-inheritance
  (with-empty-state
    ;; Humanoid inherits from inventory and arms
    (let ((obj (make-instance 'rl::humanoid)))
      (is (typep obj 'rl::inventory))
      (is (typep obj 'rl::arms)))))

(test moveable-class
  (with-empty-state
    ;; moveable inherits from pos and cooldown
    (let ((obj (make-instance 'rl::moveable :x 5 :y 5)))
      (is (typep obj 'rl::pos))
      (is (typep obj 'rl::cooldown))
      (is (= (rl::x obj) 5))
      (is (= (rl::cooldown obj) 0)))))

(test alive-class
  (with-empty-state
    ;; alive is abstract - use a concrete subclass like player
    (let ((obj (make-instance 'rl::player)))
      (is (typep obj 'rl::alive))
      (is (typep obj 'rl::moveable))
      ;; Check slots are properly initialized
      (is (numberp (rl::vitality obj)))
      (is (numberp (rl::strength obj)))
      (is (numberp (rl::health obj)))
      (is (numberp (rl::stamina obj))))))

(test can-see-accessor
  (with-empty-state
    (let ((obj (make-instance 'rl::visible :x 0 :y 0)))
      ;; Test getter and setter
      (setf (rl::can-see obj) t)
      (is (eq (rl::can-see obj) t)))))

(test add-action-method
  (with-empty-state
    (let ((obj (make-instance 'rl::visible :x 0 :y 0)))
      ;; Test add-action
      (rl::add-action obj '(:test-action))
      (is (member '(:test-action) (rl::actions obj) :test #'equal)))))

(test cool-down-method
  (with-empty-state
    ;; cool-down method needs an object with stamina - use player
    (let ((obj (make-instance 'rl::player)))
      (setf (rl::cooldown obj) 5)
      (let ((start-stamina (rl::stamina obj)))
        ;; cool-down should decrement cooldown
        (rl::cool-down obj)
        (is (= (rl::cooldown obj) 4))
        ;; cool-down should NOT regenerate stamina
        (is (= (rl::stamina obj) start-stamina))))))

(test calculate-max-health
  (with-empty-state
    (let ((obj (make-instance 'rl::player)))
      ;; Should return a number
      (is (numberp (rl::calculate-max-health obj))))))

(test calculate-max-stamina
  (with-empty-state
    (let ((obj (make-instance 'rl::player)))
      ;; Should return a number
      (is (numberp (rl::calculate-max-stamina obj))))))

(test useable-mixin
  (with-empty-state
    ;; useable requires use-cooldown - test with concrete class
    (let ((obj (make-instance 'rl::health-potion :x 0 :y 0)))
      (is (typep obj 'rl::useable))
      (is (numberp (rl::use-cooldown obj))))))

(test rechargeable-mixin
  (with-empty-state
    ;; rechargeable requires charges and use-cooldown - test with concrete class
    (let ((obj (make-instance 'rl::health-potion :x 0 :y 0)))
      (is (typep obj 'rl::rechargeable))
      (is (typep obj 'rl::useable))
      (is (numberp (rl::max-charges obj))))))

(test damage-mixin
  (with-empty-state
    ;; Test with concrete weapon class
    (let ((obj (make-instance 'rl::dagger)))
      (is (typep obj 'rl::damage))
      (is (numberp (rl::damage obj))))))

(test stamina-use-mixin  
  (with-empty-state
    ;; Test with concrete weapon class
    (let ((obj (make-instance 'rl::dagger)))
      (is (typep obj 'rl::stamina-use))
      (is (numberp (rl::stamina-use obj))))))

(test stamina-regeneration
  (with-empty-state
    ;; Test that stamina regenerates in update but not in cool-down
    (let ((player (make-instance 'rl::player)))
      ;; Reduce stamina
      (setf (rl::stamina player) (- (rl::stamina player) 10))
      (let ((reduced-stamina (rl::stamina player)))
        ;; Update should regenerate stamina
        (rl::update player)
        (is (= (rl::stamina player) (+ reduced-stamina 1)))
        
        ;; Reset and test cool-down doesn't regenerate
        (setf (rl::stamina player) reduced-stamina)
        (setf (rl::cooldown player) 5)
        (rl::cool-down player)
        (is (= (rl::stamina player) reduced-stamina))
        (is (= (rl::cooldown player) 4))))))

(test terrain-movement-modifier
  (with-empty-state
    ;; Create a moveable object
    (let ((obj (make-instance (rl::mix 'rl::moveable 'rl::pos) :x 5 :y 5 :dx 1 :dy 0)))
      (rl::add-object obj)
      
      ;; Create terrain with cooldown modifier at destination
      (let ((sand (rl::make-sand 6 5)))  ; Sand has 1.2x cooldown modifier
        (rl::add-object sand)
        
        ;; Update the object to move onto sand
        (rl::update obj)
        
        ;; Verify movement happened
        (is (= (rl::x obj) 6))
        (is (= (rl::y obj) 5))
        
        ;; Verify cooldown was modified by terrain (should be > base cooldown)
        (is (> (rl::cooldown obj) 0))))))

(test variable-shadowing-regression
  (with-empty-state
    ;; This test ensures the 'pos' variable shadowing bug doesn't return
    ;; The bug was: (if-let ((pos (get-terrain-at-pos (pos x y)))) ...)
    ;; where 'pos' variable shadowed the 'pos' function
    (let ((obj (make-instance (rl::mix 'rl::moveable 'rl::pos) :x 5 :y 5 :dx 1 :dy 1)))
      (rl::add-object obj)
      
      ;; Create some terrain
      (rl::add-object (rl::make-grass 6 6))
      
      ;; This should not error with "Invalid function name: #<RL::POS ...>"
      (finishes (rl::update obj))
      
      ;; Verify object moved
      (is (= (rl::x obj) 6))
      (is (= (rl::y obj) 6)))))