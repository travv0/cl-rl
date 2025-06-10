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

(test player-stats-initialization
  (with-empty-state
    (let ((player (make-instance 'rl::player)))
      ;; Test all stats are initialized to 5
      (is (= (rl::strength player) 5))
      (is (= (rl::dexterity player) 5))
      (is (= (rl::endurance player) 5))
      (is (= (rl::vitality player) 5))
      (is (= (rl::resistance player) 5))
      (is (= (rl::intelligence player) 5))
      (is (= (rl::faith player) 5)))))

(test update-can-see-with-opaque-objects
  (with-empty-state
    (let ((player (make-instance 'rl::player :x 10 :y 10)))
      (rl::add-object player)
      (setf rl::*player* player)
      
      ;; Add wall that blocks vision
      (rl::add-object (rl::make-wall 12 10))
      ;; Add object behind wall
      (rl::add-object (rl::make-grass 14 10))
      
      (rl::update-can-see player)
      
      ;; Wall should be visible
      (let ((wall (find-if (lambda (obj) (typep obj 'rl::wall))
                          (rl::get-objects-at-pos (rl::pos 12 10)))))
        (is (not (null wall)))
        (is (rl::can-see wall)))
      
      ;; Grass behind wall should not be visible
      (let ((grass (find-if (lambda (obj) (typep obj 'rl::grass))
                           (rl::get-objects-at-pos (rl::pos 14 10)))))
        (when grass
          (is (not (rl::can-see grass))))))))

(test update-can-see-corner-visibility
  (with-empty-state
    (let ((player (make-instance 'rl::player :x 10 :y 10)))
      (rl::add-object player)
      (setf rl::*player* player)
      
      ;; Create L-shaped wall
      (rl::add-object (rl::make-wall 12 10))
      (rl::add-object (rl::make-wall 12 11))
      (rl::add-object (rl::make-wall 12 12))
      
      ;; Add grass adjacent to wall corner
      (rl::add-object (rl::make-grass 13 11))
      
      (rl::update-can-see player)
      
      ;; Wall corners should be visible when adjacent spaces are visible
      (let ((corner-wall (find-if (lambda (obj) (typep obj 'rl::wall))
                                 (rl::get-objects-at-pos (rl::pos 12 11)))))
        (is (not (null corner-wall)))
        (is (rl::can-see corner-wall))))))

(test player-movement-and-position
  (with-empty-state
    (let ((player (make-instance 'rl::player :x 10 :y 10)))
      (rl::add-object player)
      
      ;; Test initial position
      (is (= (rl::x player) 10))
      (is (= (rl::y player) 10))
      
      ;; Test movement deltas
      (setf (rl::dx player) 1)
      (setf (rl::dy player) -1)
      (is (= (rl::dx player) 1))
      (is (= (rl::dy player) -1))
      
      ;; Update position
      (rl::update player)
      (is (= (rl::x player) 11))
      (is (= (rl::y player) 9)))))

(test player-health-potion-charges
  (with-empty-state
    (let ((player (make-instance 'rl::player)))
      ;; Check that the default health potion has 5 charges
      (let* ((inventory (rl::inventory player))
             (first-item (cdr (first inventory))))
        (is (typep first-item 'rl::health-potion))
        (is (= (rl::current-charges first-item) 5))))))

(test player-view-distance-affects-visibility
  (with-empty-state
    (let ((player (make-instance 'rl::player :x 10 :y 10)))
      (rl::add-object player)
      (setf rl::*player* player)
      
      ;; Add object at edge of view distance
      (rl::add-object (rl::make-grass (+ 10 39) 10))
      ;; Add object beyond view distance
      (rl::add-object (rl::make-grass (+ 10 41) 10))
      
      (rl::update-can-see player)
      
      ;; Object within view distance should be visible
      (let ((near-grass (find-if (lambda (obj) (typep obj 'rl::grass))
                                (rl::get-objects-at-pos (rl::pos 49 10)))))
        (when near-grass
          (is (rl::can-see near-grass))))
      
      ;; Object beyond view distance should not be visible
      (let ((far-grass (find-if (lambda (obj) (typep obj 'rl::grass))
                               (rl::get-objects-at-pos (rl::pos 51 10)))))
        (when far-grass
          (is (not (rl::can-see far-grass))))))))

(test player-max-health-calculation
  (with-empty-state
    (let ((player (make-instance 'rl::player)))
      ;; Max health should be calculated based on vitality
      (is (numberp (rl::max-health player)))
      (is (> (rl::max-health player) 0))
      ;; Current health should equal max health initially
      (is (= (rl::health player) (rl::max-health player))))))

(test player-max-stamina-calculation
  (with-empty-state
    (let ((player (make-instance 'rl::player)))
      ;; Max stamina should be calculated based on endurance
      (is (numberp (rl::max-stamina player)))
      (is (> (rl::max-stamina player) 0))
      ;; Current stamina should equal max stamina initially
      (is (= (rl::stamina player) (rl::max-stamina player))))))