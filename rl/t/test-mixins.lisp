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
      ;; cool-down should decrement cooldown
      (rl::cool-down obj)
      (is (= (rl::cooldown obj) 4)))))

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
    ;; useable requires use-cooldown
    (let ((obj (make-instance 'rl::useable :use-cooldown 10)))
      (is (typep obj 'rl::useable))
      (is (= (rl::use-cooldown obj) 10)))))

(test rechargeable-mixin
  (with-empty-state
    ;; rechargeable requires charges and use-cooldown
    (let ((obj (make-instance 'rl::rechargeable 
                              :use-cooldown 10
                              :charges 3)))
      (is (typep obj 'rl::rechargeable))
      (is (typep obj 'rl::useable))
      (is (= (rl::max-charges obj) 3)))))

(test damage-mixin
  (with-empty-state
    ;; damage requires damage argument
    (let ((obj (make-instance 'rl::damage :damage 5)))
      (is (typep obj 'rl::damage))
      (is (= (rl::damage obj) 5)))))

(test stamina-use-mixin  
  (with-empty-state
    ;; Note: stamina-use uses :damage as initarg
    (let ((obj (make-instance 'rl::stamina-use :damage 20)))
      (is (typep obj 'rl::stamina-use))
      (is (= (rl::stamina-use obj) 20)))))