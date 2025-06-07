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
      (is (numberp (slot-value player 'rl::strength)))
      (is (numberp (slot-value player 'rl::vitality)))
      (is (numberp (rl::health player)))
      (is (numberp (rl::stamina player))))))

(test update-can-see
  (with-empty-state
    ;; update-can-see requires a position argument
    (let ((player (make-instance 'rl::player :x 5 :y 5))
          (pos (rl::pos 5 5)))
      (rl::add-object player)
      (setf rl::*player* player)
      ;; Should not error when called with position
      (rl::update-can-see pos))))

(test player-is-humanoid
  (with-empty-state
    (let ((player (make-instance 'rl::player)))
      ;; Player is humanoid, so has inventory and arms
      (is (typep player 'rl::inventory))
      (is (typep player 'rl::arms))
      ;; Should have inventory slot with default health potion
      (is (listp (slot-value player 'rl::inventory)))
      (is (not (null (slot-value player 'rl::inventory)))))))

(test player-is-alive
  (with-empty-state
    (let ((player (make-instance 'rl::player)))
      ;; Player is alive, so has health and stamina
      (is (typep player 'rl::alive))
      ;; Check if slots exist and are initialized
      (is (slot-boundp player 'rl::health))
      (is (slot-boundp player 'rl::stamina))
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
      (is (= (slot-value player 'rl::view-distance) 40)))))

(test player-solidity
  (with-empty-state
    (let ((player (make-instance 'rl::player)))
      ;; Player is solid
      (is (typep player 'rl::solid)))))

(test player-default-inventory
  (with-empty-state
    (let ((player (make-instance 'rl::player)))
      ;; Player starts with health potion
      (let ((inventory (slot-value player 'rl::inventory)))
        (is (listp inventory))
        (is (not (null inventory)))
        ;; First item should be health potion
        (let ((first-item (cdr (first inventory))))
          (is (typep first-item 'rl::health-potion)))))))