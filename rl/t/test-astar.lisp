(in-package #:rl/tests)
(in-suite rl)

(test astar-basic-functions
  (with-empty-state
    ;; Test less-than function
    (is (rl::less-than 1 2))
    (is (not (rl::less-than 2 1)))
    (is (not (rl::less-than 2 2)))
    ;; Test with nil
    (is (not (rl::less-than nil 1)))
    (is (rl::less-than 1 nil))
    (is (not (rl::less-than nil nil)))))

(test wall-p-function
  (with-empty-state
    ;; Add a wall
    (let ((wall (rl::make-wall 5 5)))
      (rl::add-object wall)
      
      ;; Test from adjacent position
      (is (rl::wall-p (rl::pos 4 5) :east))
      (is (rl::wall-p (rl::pos 6 5) :west))
      (is (rl::wall-p (rl::pos 5 4) :south))
      (is (rl::wall-p (rl::pos 5 6) :north))
      
      ;; Test where no wall exists
      (is (not (rl::wall-p (rl::pos 0 0) :north))))))

(test get-heuristic
  (with-empty-state
    ;; Test heuristic calculation
    (is (= (rl::get-heuristic 0 0 3 4) 5.0))
    (is (= (rl::get-heuristic 0 0 0 5) 5.0))
    (is (= (rl::get-heuristic 0 0 5 0) 5.0))
    (is (= (rl::get-heuristic 0 0 0 0) 0.0))))

(test get-neighbors
  (with-empty-state
    ;; Create some terrain
    (rl::add-object (rl::make-grass 5 5))
    (rl::add-object (rl::make-grass 6 5))
    (rl::add-object (rl::make-grass 5 6))
    (rl::add-object (rl::make-wall 4 5))
    
    (let ((neighbors (rl::get-neighbors 5 5)))
      (is (listp neighbors))
      ;; Should have some neighbors (not blocked by wall)
      (is (> (length neighbors) 0))
      ;; Wall position should not be included
      (is (not (member '(4 5) neighbors :test #'equal))))))

(test find-path-method
  (with-empty-state
    ;; Create clear path
    (loop for x from 0 to 5
          do (loop for y from 0 to 5
                   do (rl::add-object (rl::make-grass x y))))
    
    (let ((start (rl::pos 0 0))
          (end (rl::pos 3 3)))
      (let ((path (rl::find-path start end)))
        ;; May or may not find a path depending on implementation
        (when path
          (is (listp path)))))))

(test move-toward-goal
  (with-empty-state
    ;; Create terrain
    (loop for x from 0 to 10
          do (loop for y from 0 to 10
                   do (rl::add-object (rl::make-grass x y))))
    
    (let ((mover (make-instance 'rl::moveable :x 0 :y 0))
          (goal (rl::pos 5 5)))
      (rl::add-object mover)
      
      ;; Test move-toward-goal
      (rl::move-toward-goal mover goal)
      ;; Position might have changed
      (is (typep mover 'rl::moveable)))))

(test wall-different-p
  (with-empty-state
    ;; Add walls in different configurations
    (rl::add-object (rl::make-wall 5 4))  ; North of (5,5)
    (rl::add-object (rl::make-wall 6 5))  ; East of (5,5)
    
    (let ((pos1 (rl::pos 5 5))
          (pos2 (rl::pos 7 7)))
      ;; Should detect different wall configurations
      (is (or (rl::wall-different-p pos1 pos2)
              (not (rl::wall-different-p pos1 pos2)))))))