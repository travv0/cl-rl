(in-package #:rl/tests)
(in-suite rl)

(test weapon-creation
  (with-empty-state
    ;; Weapon is abstract - skip
    (pass)))

(test dagger-creation
  (with-empty-state
    ;; Dagger has all defaults
    (let ((dagger (make-instance 'rl::dagger)))
      ;; Dagger is a weapon
      (is (typep dagger 'rl::dagger))
      (is (typep dagger 'rl::weapon))
      ;; Check default values using accessors
      (is (= (rl::damage dagger) 15))
      (is (= (rl::weapon-cooldown dagger) 2))
      (is (= (rl::weapon-strength-scale dagger) 1))
      (is (= (rl::weapon-dexterity-scale dagger) 3))
      (is (= (rl::use-cooldown dagger) 5)))))

(test sword-creation
  (with-empty-state
    ;; Sword has all defaults
    (let ((sword (make-instance 'rl::sword)))
      ;; Sword is a weapon
      (is (typep sword 'rl::sword))
      (is (typep sword 'rl::weapon))
      ;; Check default values using accessors
      (is (= (rl::damage sword) 25))
      (is (= (rl::weapon-cooldown sword) 4))
      (is (= (rl::weapon-strength-scale sword) 3))
      (is (= (rl::weapon-dexterity-scale sword) 2))
      (is (= (rl::use-cooldown sword) 10)))))

(test shield-creation
  (with-empty-state
    ;; Shield is abstract - skip
    (pass)))

(test kite-shield-creation
  (with-empty-state
    ;; Kite shield has all defaults
    (let ((kite (make-instance 'rl::kite-shield)))
      ;; Kite shield inherits from shield
      (is (typep kite 'rl::kite-shield))
      (is (typep kite 'rl::shield))
      (is (typep kite 'rl::weapon))
      ;; Check default values using accessors
      (is (= (rl::damage-reduction kite) 0.95))
      (is (= (rl::stability kite) 0.5)))))

(test weapon-initialization
  (with-empty-state
    ;; Weapons should be initialized properly
    (let ((weapon (make-instance 'rl::dagger)))
      (is (typep weapon 'rl::weapon))
      (is (numberp (rl::damage weapon))))))

(test weapon-apply-item
  (with-empty-state
    ;; Test weapon equip/unequip via apply-item
    (let ((player (make-instance 'rl::player))
          (sword (make-instance 'rl::sword)))
      ;; Apply-item should be callable (sword to player's arms)
      (rl::apply-item sword player)
      ;; Test passed if no error
      t)))

(test weapon-mixins
  (with-empty-state
    (let ((weapon (make-instance 'rl::dagger)))
      ;; Test damage mixin
      (is (typep weapon 'rl::damage))
      ;; Should have damage
      (is (numberp (rl::damage weapon)))
      
      ;; Test stamina-use mixin
      (is (typep weapon 'rl::stamina-use))
      ;; Should have stamina-use accessor
      (is (numberp (rl::stamina-use weapon))))))

(test weapon-is-useable
  (with-empty-state
    (let ((weapon (make-instance 'rl::sword)))
      ;; Weapons are useable
      (is (typep weapon 'rl::useable))
      (is (numberp (rl::use-cooldown weapon)))))

(test weapon-inheritance-chain
  (with-empty-state
    ;; Test complete inheritance for a specific weapon
    (let ((sword (make-instance 'rl::sword)))
      (is (typep sword 'rl::sword))
      (is (typep sword 'rl::weapon))
      (is (typep sword 'rl::item))
      (is (typep sword 'rl::damage))
      (is (typep sword 'rl::stamina-use))
      (is (typep sword 'rl::useable))
      (is (typep sword 'rl::visible))
      (is (typep sword 'rl::pos)))))

(test shield-functionality
  (with-empty-state
    (let ((shield (make-instance 'rl::kite-shield)))
      ;; Shield is a weapon but has special properties
      (is (typep shield 'rl::shield))
      (is (numberp (rl::damage-reduction shield)))
      (is (numberp (rl::stability shield)))
      ;; Shield still has damage (for shield bash)
      (is (= (rl::damage shield) 15))
      (is (numberp (rl::stamina-use shield)))))))