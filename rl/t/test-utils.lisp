(in-package #:rl/tests)
(in-suite rl)

(test letters-constant
  (with-empty-state
    ;; Test that +letters+ contains all lowercase letters
    (is (= (length rl::+letters+) 26))
    (is (equal (first rl::+letters+) #\a))
    (is (equal (car (last rl::+letters+)) #\z))
    ;; Check all letters are present
    (loop for c from (char-code #\a) to (char-code #\z)
          for letter in rl::+letters+
          do (is (equal letter (code-char c))))))

(test define-class-macro-basic
  (with-empty-state
    ;; Test that define-class creates valid classes
    ;; We'll use a temporary class for testing
    (let ((test-class-name (gensym "TEST-CLASS-")))
      (eval `(rl::define-class ,test-class-name ()
               ((slot1 :initarg :slot1 :accessor slot1 :initform nil)
                (slot2 :initarg :slot2 :accessor slot2 :initform 42))))
      
      ;; Create instance and test basic functionality
      (let ((obj (make-instance test-class-name :slot1 "test" :slot2 99)))
        (is (equal (slot-value obj 'slot1) "test"))
        (is (= (slot-value obj 'slot2) 99))))))

(test class-persistent-slots-method
  (with-empty-state
    ;; Test that the class-persistent-slots method exists and returns a list
    (let ((pos (make-instance 'rl::pos :x 10 :y 20)))
      (let ((slots (ms:class-persistent-slots pos)))
        (is (listp slots))
        ;; Should have at least x and y slots
        (is (>= (length slots) 2))))))

(test mixin-object-persistent-slots
  (with-empty-state
    ;; Test that mixin objects return their slots correctly
    (let ((player (make-instance 'rl::player)))
      (let ((slots (ms:class-persistent-slots player)))
        (is (listp slots))
        ;; Player should have many slots from all mixins
        (is (> (length slots) 10))))))

(test letters-usage-in-code
  (with-empty-state
    ;; Test that +letters+ can be used as expected
    (is (member #\a rl::+letters+))
    (is (member #\z rl::+letters+))
    (is (not (member #\A rl::+letters+)))
    (is (not (member #\1 rl::+letters+)))))

(test define-class-with-inheritance
  (with-empty-state
    ;; Test that define-class works with inheritance
    (let ((base-class (gensym "BASE-"))
          (derived-class (gensym "DERIVED-")))
      (eval `(rl::define-class ,base-class ()
               ((base-slot :initarg :base-slot :accessor base-slot :initform "base"))))
      (eval `(rl::define-class ,derived-class (,base-class)
               ((derived-slot :initarg :derived-slot :accessor derived-slot :initform "derived"))))
      
      (let ((obj (make-instance derived-class)))
        (is (equal (slot-value obj 'base-slot) "base"))
        (is (equal (slot-value obj 'derived-slot) "derived"))))))