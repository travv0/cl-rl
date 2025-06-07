(in-package #:rl/tests)
(in-suite rl)

(test terrain-creation
  (with-empty-state
    (let ((terrain (make-instance 'rl::terrain)))
      (is (typep terrain 'rl::terrain)))))

(test wall-creation
  (with-empty-state
    (let ((wall (rl::make-wall 5 5)))
      (is (typep wall 'rl::wall))
      (is (typep wall 'rl::visible))
      (is (typep wall 'rl::solid))
      (is (typep wall 'rl::opaque))
      (is (= (rl::x wall) 5))
      (is (= (rl::y wall) 5)))))

(test grass-creation
  (with-empty-state
    (let ((grass (rl::make-grass 3 4)))
      (is (typep grass 'rl::grass))
      (is (typep grass 'rl::terrain))
      (is (typep grass 'rl::visible))
      (is (= (rl::x grass) 3))
      (is (= (rl::y grass) 4)))))

(test tree-creation
  (with-empty-state
    (let ((tree (rl::make-tree 7 8)))
      (is (typep tree 'rl::tree))
      (is (typep tree 'rl::visible))
      (is (typep tree 'rl::solid))
      (is (typep tree 'rl::opaque)))))

(test water-creation
  (with-empty-state
    (let ((water (rl::make-water 1 1)))
      (is (typep water 'rl::water))
      (is (typep water 'rl::visible))
      (is (typep water 'rl::solid)))))

(test shallow-water-creation
  (with-empty-state
    (let ((shallow (make-instance 'rl::shallow-water)))
      (is (typep shallow 'rl::shallow-water))
      (is (typep shallow 'rl::terrain))
      (is (typep shallow 'rl::visible)))))

(test sand-creation
  (with-empty-state
    (let ((sand (rl::make-sand 2 3)))
      (is (typep sand 'rl::sand))
      (is (typep sand 'rl::terrain))
      (is (typep sand 'rl::visible)))))

(test spawn-creation
  (with-empty-state
    (let ((spawn (rl::make-spawn 0 0)))
      (is (typep spawn 'rl::spawn))
      (is (typep spawn 'rl::pos))
      (is (= (rl::x spawn) 0))
      (is (= (rl::y spawn) 0)))))

(test door-creation
  (with-empty-state
    (let ((door (rl::make-door 5 5)))
      (is (typep door 'rl::door))
      (is (typep door 'rl::visible))
      ;; Door is dynamically mixed with solid and opaque
      (is (typep door 'rl::solid))
      (is (typep door 'rl::opaque)))))

(test terrain-check
  (with-empty-state
    (let ((grass (make-instance 'rl::grass))
          (wall (make-instance 'rl::wall))
          (item (make-instance 'rl::item)))
      (is (rl::terrain-p grass))
      (is (not (rl::terrain-p wall)))
      (is (not (rl::terrain-p item))))))

(test get-objects-at-pos
  (with-empty-state
    (let ((obj1 (make-instance 'rl::pos :x 5 :y 5))
          (obj2 (make-instance 'rl::pos :x 5 :y 5))
          (obj3 (make-instance 'rl::pos :x 6 :y 6)))
      (rl::add-object obj1)
      (rl::add-object obj2)
      (rl::add-object obj3)
      
      (let ((objs (rl::get-objects-at-pos (rl::pos 5 5))))
        (is (= (length objs) 2))
        (is (member obj1 objs))
        (is (member obj2 objs))
        (is (not (member obj3 objs)))))))

(test get-object-at-pos
  (with-empty-state
    (let ((wall (rl::make-wall 5 5))
          (item (make-instance 'rl::item :x 5 :y 5)))
      (rl::add-object wall)
      (rl::add-object item)
      
      ;; Get specific type
      (is (eq (rl::get-object-at-pos (rl::pos 5 5) 'rl::wall) wall))
      (is (eq (rl::get-object-at-pos (rl::pos 5 5) 'rl::item) item))
      
      ;; Get any object
      (is (member (rl::get-object-at-pos (rl::pos 5 5)) (list wall item))))))

(test perlin-noise-seed
  (with-empty-state
    ;; Test it returns a string
    (is (stringp (rl::make-perlin-noise-seed 12345)))
    ;; Test different seeds give different results
    (is (not (string= (rl::make-perlin-noise-seed 12345)
                      (rl::make-perlin-noise-seed 54321))))))

(test chunk-calculations
  (with-empty-state
    ;; Test player-chunk
    (let ((player (make-instance 'rl::player :x 150 :y 250)))
      (setf rl::*player* player)
      (let ((chunk (rl::player-chunk)))
        (is (typep chunk 'rl::pos))
        (is (= (rl::x chunk) 100))  ; floor(150/100)*100
        (is (= (rl::y chunk) 200))))) ; floor(250/100)*100
  
  ;; Test chunks-to-show
  (let ((chunks (rl::chunks-to-show)))
    (is (listp chunks))
    ;; Should return list of chunk positions
    (dolist (chunk chunks)
      (is (typep chunk 'rl::pos)))))

(test should-display
  (with-empty-state
    ;; Test base method - returns t
    (let ((obj (make-instance 'rl::pos)))
      (is (rl::should-display obj))))) 