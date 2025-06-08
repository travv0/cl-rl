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
      (rl::apply-item player potion)
      ;; Health should increase
      (is (> (rl::health player) 50))
      ;; Potion should be deleted after use
      (is (typep potion 'rl::deleted)))))

(test item-stacking
  (with-empty-state
    ;; Test adding stackable items
    (let ((inv-obj (make-instance 'rl::inventory))
          (potion1 (make-instance 'rl::health-potion))
          (potion2 (make-instance 'rl::health-potion)))
      ;; Add first potion
      (rl::add-to-inventory inv-obj potion1 #\a)
      ;; Add second potion to same slot
      (rl::add-to-inventory inv-obj potion2 #\a)
      
      ;; Should have one slot with list of items
      (is (= (length (slot-value inv-obj 'rl::inventory)) 1))
      (let ((slot-value (cdr (assoc #\a (slot-value inv-obj 'rl::inventory)))))
        (is (listp slot-value))
        (is (= (length slot-value) 2))
        (is (member potion1 slot-value))
        (is (member potion2 slot-value)))))))