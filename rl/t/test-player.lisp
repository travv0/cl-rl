(in-package #:rl/tests)
(in-suite rl)

(test player-creation
  (with-empty-state
    ;; Player can be created without arguments (has defaults)
    (let ((player (make-instance 'rl::player)))
      ;; Test inheritance
      (is (typep player 'rl::alive))
      (is (typep player 'rl::visible))
      (is (typep player 'rl::solid))
      (is (typep player 'rl::humanoid))
      
      ;; Has default position
      (is (numberp (rl::x player)))
      (is (numberp (rl::y player)))
      
      ;; Has default stats
      (is (numberp (rl::strength player)))
      (is (numberp (rl::vitality player)))
      (is (numberp (rl::health player)))
      (is (numberp (rl::stamina player))))))

(test update-can-see
  (with-empty-state
    ;; update-can-see requires a position argument
    (let ((player (make-instance 'rl::player :x 5 :y 5))
          (pos (rl::pos 5 5)))
      (rl::add-object player)
      (setf rl::*player* player)
      ;; Should not error when called with player
      (rl::update-can-see player))))

(test player-is-humanoid
  (with-empty-state
    (let ((player (make-instance 'rl::player)))
      ;; Player is humanoid, so has inventory and arms
      (is (typep player 'rl::inventory))
      (is (typep player 'rl::arms))
      ;; Should have inventory slot with default health potion
      (is (listp (rl::inventory player)))
      (is (not (null (rl::inventory player)))))))

(test player-is-alive
  (with-empty-state
    (let ((player (make-instance 'rl::player)))
      ;; Player is alive, so has health and stamina
      (is (typep player 'rl::alive))
      ;; Check if slots exist and are initialized through readers
      (is (numberp (rl::health player)))
      (is (numberp (rl::stamina player)))
      (is (numberp (rl::health player)))
      (is (numberp (rl::stamina player))))))

(test player-visibility
  (with-empty-state
    (let ((player (make-instance 'rl::player :x 10 :y 10)))
      ;; Player is visible
      (is (typep player 'rl::visible))
      ;; Has can-see slot accessor
      (setf (rl::can-see player) '())
      (is (null (rl::can-see player)))
      ;; Has view-distance
      (is (= (rl::view-distance player) 40)))))

(test player-solidity
  (with-empty-state
    (let ((player (make-instance 'rl::player)))
      ;; Player is solid
      (is (typep player 'rl::solid)))))

(test player-default-inventory
  (with-empty-state
    (let ((player (make-instance 'rl::player)))
      ;; Player starts with health potion
      (let ((inventory (rl::inventory player)))
        (is (listp inventory))
        (is (not (null inventory)))
        ;; First item should be health potion
        (let ((first-item (cdr (first inventory))))
          (is (typep first-item 'rl::health-potion)))))))

(test update-can-see-variable-shadowing
  (with-empty-state
    ;; This test ensures the 'pos' loop variable doesn't shadow the pos function
    ;; The bug was: for pos in (rest (get-line from (pos x y)))
    (let ((player (make-instance 'rl::player :x 50 :y 50)))
      (rl::add-object player)
      (setf rl::*player* player)
      
      ;; Add some visible objects
      (rl::add-object (rl::make-grass 52 50))
      (rl::add-object (rl::make-wall 55 50))
      
      ;; This should not error with "Invalid function name: #<RL::POS ...>"
      (finishes (rl::update-can-see player))
      
      ;; Verify visibility was calculated
      (is (rl::can-see player))
      ;; Grass should be visible
      (is (rl::can-see (first (rl::get-objects-at-pos (rl::pos 52 50))))))))