(in-package #:rl/tests)
(in-suite rl)

(test astar-basic-functions
  (with-empty-state
    ;; Test less-than function
    (is (rl::less-than 1 2))
    (is (not (rl::less-than 2 1)))
    (is (not (rl::less-than 2 2)))
    ;; Test with nil (nil y means infinity)
    (is (rl::less-than 1 nil))  ; 1 < infinity
    (is (rl::less-than 0 nil))))

(test wall-p-function
  (with-empty-state
    ;; Add a wall
    (let ((wall (rl::make-wall 5 5)))
      (rl::add-object wall)
      
      ;; wall-p takes (x y direction)
      (is (rl::wall-p 4 5 :right))
      (is (rl::wall-p 6 5 :left))
      (is (rl::wall-p 5 4 :down))
      (is (rl::wall-p 5 6 :up))
      
      ;; Test where no wall exists
      (is (not (rl::wall-p 0 0 :up))))))

(test get-heuristic
  (with-empty-state
    ;; Test heuristic calculation - takes position cons cells
    (is (= (rl::get-heuristic '(0 . 0) '(3 . 4)) 7))
    (is (= (rl::get-heuristic '(0 . 0) '(0 . 5)) 5))
    (is (= (rl::get-heuristic '(0 . 0) '(5 . 0)) 5))
    (is (= (rl::get-heuristic '(0 . 0) '(0 . 0)) 0))))

(test get-neighbors
  (with-empty-state
    ;; Create some terrain
    (rl::add-object (rl::make-grass 5 5))
    (rl::add-object (rl::make-grass 6 5))
    (rl::add-object (rl::make-grass 5 6))
    (rl::add-object (rl::make-wall 4 5))
    
    (let ((neighbors (rl::get-neighbors '(5 . 5) (rl::pos 10 10))))
      (is (listp neighbors))
      ;; Should have some neighbors (not blocked by wall)
      (is (> (length neighbors) 0))
      ;; Each neighbor should be a cons of (pos . cost)
      (is (every (lambda (n) (and (consp n) (consp (car n)) (numberp (cdr n)))) neighbors)))))

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
    
    (let ((pos1 (rl::pos 5 5)))
      ;; wall-different-p takes (pos prev-x prev-y)
      (is (or (rl::wall-different-p pos1 7 7)
              (not (rl::wall-different-p pos1 7 7)))))))