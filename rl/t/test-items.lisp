(in-package #:rl/tests)
(in-suite rl)

(test item-creation
  (with-empty-state
    ;; Item requires position
    (let ((item (make-instance 'rl::item :x 5 :y 5)))
      ;; Item inherits from visible
      (is (typep item 'rl::visible))
      (is (typep item 'rl::pos))
      (is (= (rl::x item) 5))
      (is (= (rl::y item) 5)))))

(test health-potion-creation
  (with-empty-state
    ;; Health potion has all defaults
    (let ((potion (make-instance 'rl::health-potion)))
      ;; Health potion has multiple inheritance
      (is (typep potion 'rl::health-potion))
      (is (typep potion 'rl::rechargeable))
      (is (typep potion 'rl::useable))
      (is (typep potion 'rl::item))
      (is (typep potion 'rl::visible))
      ;; Check default values
      (is (= (rl::use-cooldown potion) 15))
      (is (= (rl::regeneration-amount potion) 40))
      (is (= (rl::max-charges potion) 5)))))

(test make-inventory-function
  (with-empty-state
    ;; Test empty inventory
    (let ((inv1 (rl::make-inventory)))
      (is (listp inv1))
      (is (null inv1)))
    
    ;; Test with items
    (let* ((item1 (make-instance 'rl::item :x 0 :y 0))
           (item2 (make-instance 'rl::item :x 0 :y 0))
           (inv2 (rl::make-inventory item1 item2)))
      (is (listp inv2))
      (is (= (length inv2) 2))
      ;; Should be alist with keys #\a, #\b, etc
      (is (eq (caar inv2) #\a))
      (is (eq (caadr inv2) #\b)))))

(test add-to-inventory
  (with-empty-state
    (let ((inv-obj (make-instance 'rl::inventory))
          (item (make-instance 'rl::item :x 0 :y 0)))
      ;; Add item to inventory
      (rl::add-to-inventory item inv-obj)
      (is (assoc #\a (rl::inventory inv-obj)))
      (is (eq (cdr (assoc #\a (rl::inventory inv-obj))) item)))))

(test apply-item-base
  (with-empty-state
    ;; Base apply-item method exists
    (let ((player (make-instance 'rl::player))
          (item (make-instance 'rl::item :x 0 :y 0)))
      ;; Should be callable (may do nothing)
      (rl::apply-item player item))))

(test rechargeable-initialization
  (with-empty-state
    (let ((rechargeable (make-instance 'rl::health-potion)))
      ;; Should have charges initialized
      (is (numberp (rl::current-charges rechargeable)))
      (is (numberp (rl::max-charges rechargeable)))
      (is (= (rl::current-charges rechargeable) (rl::max-charges rechargeable))))))

(test inventory-operations
  (with-empty-state
    (let ((inv-obj (make-instance 'rl::inventory)))
      ;; Inventory should be initialized as empty list
      (is (listp (rl::inventory inv-obj)))
      
      ;; Add multiple items
      (let ((item1 (make-instance 'rl::item :x 0 :y 0))
            (item2 (make-instance 'rl::item :x 0 :y 0)))
        (rl::add-to-inventory item1 inv-obj)
        (rl::add-to-inventory item2 inv-obj)
        
        (is (= (length (rl::inventory inv-obj)) 2))
        (is (eq (cdr (assoc #\a (rl::inventory inv-obj))) item1))
        (is (eq (cdr (assoc #\b (rl::inventory inv-obj))) item2)))))

(test item-apply-methods
  (with-empty-state
    ;; Test that health potion has proper apply method
    (let ((player (make-instance 'rl::player))
          (potion (make-instance 'rl::health-potion)))
      ;; Set player health to less than max
      (setf (rl::health player) 50)
      ;; Apply potion
      (rl::apply-item potion player)
      ;; Health should increase
      (is (> (rl::health player) 50)))))

(test item-stacking
  (with-empty-state
    ;; Test adding stackable items
    (let ((inv-obj (make-instance 'rl::inventory))
          (potion1 (make-instance 'rl::health-potion))
          (potion2 (make-instance 'rl::health-potion)))
      ;; Add first potion
      (rl::add-to-inventory potion1 inv-obj)
      ;; Add second potion - should get different slot
      (rl::add-to-inventory potion2 inv-obj)
      
      ;; Should have two slots (a and b)
      (is (= (length (rl::inventory inv-obj)) 2))
      (is (eq (cdr (assoc #\a (rl::inventory inv-obj))) potion1))
      (is (eq (cdr (assoc #\b (rl::inventory inv-obj))) potion2)))))

(test useable-class
  (with-empty-state
    ;; Test that useable requires use-cooldown
    (signals error
      (make-instance 'rl::useable))
    
    ;; Test creating a custom useable
    (let ((useable (make-instance 'rl::useable :use-cooldown 10)))
      (is (= (rl::use-cooldown useable) 10)))))

(test rechargeable-charges
  (with-empty-state
    ;; Test rechargeable requires max-charges
    (signals error
      (make-instance 'rl::rechargeable :use-cooldown 5))
    
    ;; Test rechargeable with charges
    (let ((rechargeable (make-instance 'rl::rechargeable 
                                      :charges 3
                                      :use-cooldown 5)))
      (is (= (rl::max-charges rechargeable) 3))
      (is (= (rl::current-charges rechargeable) 3))
      (is (= (rl::use-cooldown rechargeable) 5)))))

(test apply-item-with-charges
  (with-empty-state
    (let ((player (make-instance 'rl::player))
          (potion (make-instance 'rl::health-potion)))
      ;; Set player health low
      (setf (rl::health player) 50)
      
      ;; Apply potion with charges
      (is (= (rl::current-charges potion) 5))
      (rl::apply-item potion player)
      (is (= (rl::current-charges potion) 4))
      (is (> (rl::health player) 50))
      
      ;; Use up all charges
      (dotimes (i 4)
        (rl::apply-item potion player))
      (is (zerop (rl::current-charges potion)))
      
      ;; Try to use with no charges - should fail
      (let ((old-health (rl::health player)))
        (setf rl::*state* :inventory) ; Set state to test it gets reset
        (rl::apply-item potion player)
        (is (= (rl::health player) old-health)) ; Health unchanged
        (is (eq rl::*state* :play)) ; State reset to play
        ;; Check log message
        (is (search "out of charges" (first rl::*log*)))))))

(test apply-item-cooldown-effect
  (with-empty-state
    (let ((player (make-instance 'rl::player))
          (potion (make-instance 'rl::health-potion)))
      ;; Player should have 0 cooldown initially
      (is (zerop (rl::cooldown player)))
      
      ;; Apply potion
      (rl::apply-item potion player)
      
      ;; Cooldown should increase by use-cooldown
      (is (= (rl::cooldown player) (rl::use-cooldown potion))))))

(test apply-item-log-messages
  (with-empty-state
    (let ((player (make-instance 'rl::player))
          (potion (make-instance 'rl::health-potion)))
      ;; Clear log
      (setf rl::*log* nil)
      
      ;; Apply potion
      (rl::apply-item potion player)
      
      ;; Should have log message about drinking
      (is (not (null rl::*log*)))
      (let ((log-msg (first rl::*log*)))
        (is (search "drank" log-msg))))))

(test apply-item-state-reset
  (with-empty-state
    (let ((player (make-instance 'rl::player))
          (potion (make-instance 'rl::health-potion)))
      ;; Set state to inventory
      (setf rl::*state* :inventory)
      
      ;; Apply item
      (rl::apply-item potion player)
      
      ;; State should be reset to play
      (is (eq rl::*state* :play)))))

(test apply-item-incompatible
  (with-empty-state
    ;; Test applying item to incompatible object
    (let ((item (make-instance 'rl::item :x 0 :y 0))
          (obj (make-instance 'rl::pos :x 0 :y 0)))
      ;; Clear log
      (setf rl::*log* nil)
      (setf rl::*state* :inventory)
      
      ;; Apply item to non-alive object
      (rl::apply-item item obj)
      
      ;; Should log error message
      (is (search "unable to use" (first rl::*log*)))
      ;; State should still be reset
      (is (eq rl::*state* :play)))))

(test add-to-inventory-ordering
  (with-empty-state
    (let ((inv-obj (make-instance 'rl::inventory)))
      ;; Add items in reverse order
      (let ((items (loop for i from 0 to 5
                        collect (make-instance 'rl::item :x 0 :y 0))))
        ;; Add in specific order to test sorting
        (rl::add-to-inventory (nth 2 items) inv-obj) ; Should get 'a'
        (rl::add-to-inventory (nth 0 items) inv-obj) ; Should get 'b'
        (rl::add-to-inventory (nth 1 items) inv-obj) ; Should get 'c'
        
        ;; Check inventory is sorted by character
        (let ((inv (rl::inventory inv-obj)))
          (is (= (length inv) 3))
          (is (char< (car (first inv)) (car (second inv))))
          (is (char< (car (second inv)) (car (third inv))))
          ;; Verify items are assigned correct chars
          (is (eq (car (first inv)) #\a))
          (is (eq (car (second inv)) #\b))
          (is (eq (car (third inv)) #\c)))))))

(test add-to-inventory-full-alphabet
  (with-empty-state
    (let ((inv-obj (make-instance 'rl::inventory)))
      ;; Fill inventory with lowercase letters
      (loop for i from 0 to 25
            do (rl::add-to-inventory (make-instance 'rl::item :x 0 :y 0) inv-obj))
      
      ;; Inventory should have 26 items (a-z)
      (is (= (length (rl::inventory inv-obj)) 26))
      
      ;; Add more items - should use uppercase
      (rl::add-to-inventory (make-instance 'rl::item :x 0 :y 0) inv-obj)
      (is (= (length (rl::inventory inv-obj)) 27))
      (is (assoc #\A (rl::inventory inv-obj))))))

(test inventory-chars-parameter
  (with-empty-state
    ;; Test that inventory-chars is defined properly
    (is (listp rl::*inventory-chars*))
    (is (= (length rl::*inventory-chars*) 52)) ; a-z, A-Z
    (is (eq (first rl::*inventory-chars*) #\a))
    (is (eq (nth 25 rl::*inventory-chars*) #\z))
    (is (eq (nth 26 rl::*inventory-chars*) #\A))
    (is (eq (nth 51 rl::*inventory-chars*) #\Z))))

(test health-potion-healing
  (with-empty-state
    (let ((player (make-instance 'rl::player))
          (potion (make-instance 'rl::health-potion)))
      ;; Damage player
      (setf (rl::health player) 60)
      (let ((initial-health (rl::health player)))
        
        ;; Apply potion
        (rl::apply-item potion player)
        
        ;; Health should increase by regeneration-amount
        (is (= (rl::health player) 
               (+ initial-health (rl::regeneration-amount potion))))))))

(test health-potion-max-health-cap
  (with-empty-state
    (let ((player (make-instance 'rl::player))
          (potion (make-instance 'rl::health-potion)))
      ;; Set health close to max
      (setf (rl::health player) (- (rl::max-health player) 10))
      
      ;; Apply potion
      (rl::apply-item potion player)
      
      ;; Health should be capped at max
      (is (= (rl::health player) (rl::max-health player))))))

(test apply-multiple-items-cooldown
  (with-empty-state
    (let ((player (make-instance 'rl::player))
          (potion1 (make-instance 'rl::health-potion))
          (potion2 (make-instance 'rl::health-potion)))
      ;; Apply first potion
      (rl::apply-item potion1 player)
      (let ((cooldown1 (rl::cooldown player)))
        
        ;; Apply second potion
        (rl::apply-item potion2 player)
        
        ;; Cooldown should stack
        (is (= (rl::cooldown player) 
               (+ cooldown1 (rl::use-cooldown potion2))))))))

(test add-to-inventory-return-value
  (with-empty-state
    (let ((inv-obj (make-instance 'rl::inventory))
          (item (make-instance 'rl::item :x 0 :y 0)))
      ;; add-to-inventory should return the inventory
      (let ((result (rl::add-to-inventory item inv-obj)))
        (is (listp result))
        (is (eq result (rl::inventory inv-obj)))))))

(test apply-item-alive-requirement
  (with-empty-state
    ;; Test that health potion only works on alive objects
    (let ((potion (make-instance 'rl::health-potion))
          (non-alive (make-instance 'rl::item :x 0 :y 0)))
      (setf rl::*log* nil)
      
      ;; Try to apply potion to non-alive object
      (rl::apply-item potion non-alive)
      
      ;; Should get "unable to use" message
      (is (search "unable to use" (first rl::*log*)))))))