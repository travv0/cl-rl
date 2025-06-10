(in-package #:rl/tests)
(in-suite rl)

(test log-writing
  (with-empty-state
    (is (null rl::*log*))
    (rl::write-to-log "Test message")
    (is (equal (first rl::*log*) "Test message"))
    (rl::write-to-log "Message with ~a" "args")
    (is (equal (first rl::*log*) "Message with args"))))

(test display-name-generation
  (with-empty-state
    (let ((rl::*name-cache* (make-hash-table)))
      ;; Test with a basic object
      (let ((obj (make-instance 'rl::pos :x 5 :y 5)))
        (is (stringp (rl::display-name obj)))
        ;; Check caching works
        (let ((name1 (rl::display-name obj))
              (name2 (rl::display-name obj)))
          (is (string= name1 name2)))))))

(test object-management
  (with-empty-state
    (let ((obj (make-instance 'rl::pos :x 5 :y 5)))
      (is (null rl::*game-objects*))
      (rl::add-object obj)
      (is (member obj rl::*game-objects*))
      (is (member obj (aref rl::*pos-cache* 5 5)))
      
      (rl::clear-position obj)
      (is (not (member obj (aref rl::*pos-cache* 5 5)))))))

(test clear-objects
  (with-empty-state
    (let ((obj1 (make-instance 'rl::pos :x 1 :y 1))
          (obj2 (make-instance 'rl::pos :x 2 :y 2)))
      (rl::add-object obj1)
      (rl::add-object obj2)
      (is (= (length rl::*game-objects*) 2))
      
      (rl::clear-objects)
      (is (null rl::*game-objects*))
      (is (null (aref rl::*pos-cache* 1 1)))
      (is (null (aref rl::*pos-cache* 2 2))))))

(test update-mechanism
  (with-empty-state
    ;; Base update method does nothing but should be callable
    (let ((obj (make-instance 'rl::pos)))
      (is (null (rl::update obj))))))

(test primary-class-extraction
  (with-empty-state
    (let ((obj (make-instance 'rl::pos)))
      (is (symbolp (class-name (rl::primary-class-of-mixin obj)))))))

(test quit-condition
  (is (typep (make-condition 'rl::quit-condition) 'rl::quit-condition)))

(test name-cache-weak-table
  (with-empty-state
    ;; Test that name cache is a hash table
    (is (hash-table-p rl::*name-cache*))
    ;; Test that display names are cached
    (let ((obj (make-instance 'rl::item :x 0 :y 0)))
      (let ((name1 (rl::display-name obj))
            (name2 (rl::display-name obj)))
        (is (string= name1 name2))
        (is (gethash obj rl::*name-cache*))
        ;; Verify it's using object identity
        (is (eq (hash-table-test rl::*name-cache*) 'eq))))))

(test movement-processing
  "Test unified movement processing"
  (with-empty-state
    (let ((player (make-instance 'rl::player :x 10 :y 10)))
      (setf rl::*player* player)
      
      ;; Test basic movement
      (rl::process-movement -1 nil)
      (is (= (rl::dx player) -1))
      (is (= (rl::dy player) 0))
      
      ;; Test diagonal movement
      (rl::process-movement 1 -1)
      (is (= (rl::dx player) 1))
      (is (= (rl::dy player) -1))
      
      ;; Test running
      (rl::delete-from-mix player 'rl::running)
      (rl::process-movement -1 1 t)
      (is (= (rl::dx player) -1))
      (is (= (rl::dy player) 1))
      (is (typep player 'rl::running)))))

(test dump-object-methods
  "Test dump-object for different object types"
  (with-empty-state
    ;; Test visible object dump
    (let ((goblin (make-instance 'rl::goblin :x 10 :y 20)))
      (let ((dump (rl::dump-object goblin)))
        (is (listp dump))
        (is (eq (getf dump :name) :GOBLIN))
        (is (stringp (getf dump :display-name)))
        (is (= (getf dump :x) 10))
        (is (= (getf dump :y) 20))
        (is (listp (getf dump :actions)))))
    
    ;; Test door dump
    (let ((door (make-instance 'rl::door :x 5 :y 5)))
      (let ((dump (rl::dump-object door)))
        (is (listp dump))
        (is (eq (getf dump :name) :DOOR))
        (let ((attrs (getf dump :attributes)))
          (is (member :open attrs)))))
    
    ;; Test player dump
    (let ((player (make-instance 'rl::player :x 0 :y 0)))
      (setf rl::*player* player)
      (let ((dump (rl::dump-object player)))
        (is (listp dump))
        (is (eq (getf dump :name) :PLAYER))
        (let ((attrs (getf dump :attributes)))
          (is (numberp (getf attrs :health)))
          (is (numberp (getf attrs :max-health)))
          (is (numberp (getf attrs :stamina)))
          (is (numberp (getf attrs :max-stamina))))))
    
    ;; Test rechargeable dump
    (let ((potion (make-instance 'rl::health-potion :x 0 :y 0)))
      (let ((dump (rl::dump-object potion)))
        (is (listp dump))
        (let ((attrs (getf dump :attributes)))
          (is (numberp (getf attrs :charges)))
          (is (numberp (getf attrs :max-charges))))))))

(test dump-state-play
  "Test dump-state in play mode"
  (with-empty-state
    (let ((player (make-instance 'rl::player :x 50 :y 50)))
      (setf rl::*player* player
            rl::*state* :play
            rl::*seed* 12345
            rl::*turn* 5)
      (rl::add-object player)
      
      ;; Add some test objects
      (rl::add-object (make-instance 'rl::goblin :x 52 :y 52))
      (rl::add-object (make-instance 'rl::health-potion :x 51 :y 51))
      
      ;; Add a log entry
      (rl::write-to-log "Test log entry")
      
      (let ((state (rl::dump-state)))
        (is (listp state))
        (is (listp (getf state :player)))
        (is (listp (getf state :log)))
        (is (equal (first (getf state :log)) "Test log entry"))
        (is (= (getf state :turn) 5))
        (is (= (getf state :seed) 12345))
        (is (listp (getf state :objects)))))))


(test dump-state-inventory
  "Test dump-state in inventory mode"
  (with-empty-state
    (let ((player (make-instance 'rl::player)))
      (setf rl::*player* player
            rl::*state* :inventory)
      ;; Add item to inventory
      (push (cons #\a (make-instance 'rl::health-potion)) 
            (rl::inventory player))
      
      (let ((state (rl::dump-state)))
        (is (listp state))
        (is (listp (getf state :inventory)))
        (is (> (length (getf state :inventory)) 0))))))


(test get-all-subclasses
  "Test get-all-subclasses function"
  (with-empty-state
    ;; Test with enemy class
    (let ((subclasses (rl::get-all-subclasses 'rl::enemy)))
      (is (listp subclasses))
      ;; Should include goblin, warrior, rat, etc
      (is (member (find-class 'rl::goblin) subclasses))
      (is (member (find-class 'rl::warrior) subclasses))
      (is (member (find-class 'rl::rat) subclasses)))))

(test thread-safe-operations
  "Test thread-safe game state operations"
  (with-empty-state
    ;; Test sequential object additions (thread test removed due to dynamic binding issues)
    (let ((objects (loop for i from 0 below 10
                        collect (make-instance 'rl::pos :x i :y i))))
      (dolist (obj objects)
        (rl::add-object obj))
      
      ;; All objects should be added
      (is (= (length rl::*game-objects*) 10))
      (loop for i from 0 below 10
            do (is (member (nth i objects) 
                          (rl::safe-get-pos-cache-at i i)))))))

(test safe-game-state-operations
  "Test safe game state manipulation functions"
  (with-empty-state
    (let ((obj1 (make-instance 'rl::pos :x 1 :y 1))
          (obj2 (make-instance 'rl::pos :x 2 :y 2)))
      
      ;; Test safe-push-to-game-objects
      (rl::safe-push-to-game-objects obj1)
      (is (member obj1 rl::*game-objects*))
      
      ;; Test safe-push-to-pos-cache
      (rl::safe-push-to-pos-cache obj2 2 2)
      (is (member obj2 (rl::safe-get-pos-cache-at 2 2)))
      
      ;; Test safe-remove-from-game-objects
      (rl::safe-remove-from-game-objects obj1)
      (is (not (member obj1 rl::*game-objects*)))
      
      ;; Test safe-remove-from-pos-cache
      (rl::safe-remove-from-pos-cache obj2 2 2)
      (is (not (member obj2 (rl::safe-get-pos-cache-at 2 2))))
      
      ;; Test safe-clear-game-state
      (rl::add-object (make-instance 'rl::pos :x 3 :y 3))
      (rl::safe-clear-game-state)
      (is (null rl::*game-objects*))
      ;; Check that pos-cache is cleared
      (let ((all-empty t))
        (loop for x from 0 below (array-dimension rl::*pos-cache* 0)
              while all-empty
              do (loop for y from 0 below (array-dimension rl::*pos-cache* 1)
                      while all-empty
                      do (when (aref rl::*pos-cache* x y)
                           (setf all-empty nil))))
        (is (eq all-empty t))))))

(test initialize-with-seed
  "Test game initialization with seed"
  (with-empty-state
    (rl::initialize 99999)
    (is (= rl::*seed* 99999))
    (is (= rl::*turn* 1))
    (is (null rl::*log*))
    ;; Initialize generates terrain, so game-objects won't be null
    (is (listp rl::*game-objects*))
    (is (arrayp rl::*pos-cache*))
    (is (= (array-dimension rl::*pos-cache* 0) rl::*stage-width*))
    (is (= (array-dimension rl::*pos-cache* 1) rl::*stage-height*))))