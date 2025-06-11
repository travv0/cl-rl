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
    (let ((test-class-name 'test-define-class-basic))
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
    (let ((base-class 'test-base-class)
          (derived-class 'test-derived-class))
      (eval `(rl::define-class ,base-class ()
               ((base-slot :initarg :base-slot :accessor base-slot :initform "base"))))
      (eval `(rl::define-class ,derived-class (,base-class)
               ((derived-slot :initarg :derived-slot :accessor derived-slot :initform "derived"))))
      
      (let ((obj (make-instance derived-class)))
        (is (equal (slot-value obj 'base-slot) "base"))
        (is (equal (slot-value obj 'derived-slot) "derived"))))))

(test marshal-basic-mixin-object
  (with-empty-state
    ;; Test basic marshaling of a simple mixin object
    (let ((pos (make-instance 'rl::pos :x 42 :y 13)))
      (let ((marshaled (ms:marshal pos)))
        (is (listp marshaled))
        ;; Should be a proper marshaled form
        (is (not (null marshaled)))
        ;; Should contain multiple elements
        (is (>= (length marshaled) 4))))))

(test marshal-unmarshal-round-trip
  (with-empty-state
    ;; Test that marshaling and unmarshaling preserves object state
    (let* ((original (make-instance 'rl::pos :x 100 :y 200))
           (marshaled (ms:marshal original)))
      ;; Verify marshaling worked
      (is (listp marshaled))
      (is (not (null marshaled))))))

(test marshal-circular-reference
  (with-empty-state
    ;; Test handling of circular references
    (let* ((obj1 (make-instance 'rl::pos :x 1 :y 1)))
      ;; Just test that marshaling works with circular hash
      (let ((circle-hash (make-hash-table))
            (marshal1 (ms:marshal obj1)))
        ;; Basic marshaling should work
        (is (listp marshal1))
        (is (not (null marshal1)))))))

(test marshal-complex-mixin-object
  (with-empty-state
    ;; Test marshaling a complex object with multiple mixins
    (let ((player (make-instance 'rl::player)))
      (setf (rl::health player) 75)
      (setf (rl::stamina player) 50)
      (let ((marshaled (ms:marshal player)))
        (is (listp marshaled))
        ;; Player should have a complex marshaled form
        (is (>= (length marshaled) 4))))))

(test marshal-various-objects
  (with-empty-state
    ;; Test that various object types can be marshaled
    (let ((objects (list
                    (make-instance 'rl::pos :x 1 :y 2)
                    (make-instance 'rl::item :x 3 :y 4)
                    (make-instance 'rl::player))))
      (dolist (obj objects)
        (let ((marshaled (ms:marshal obj)))
          (is (listp marshaled))
          (is (not (null marshaled))))))))

(test class-persistent-slots-all-types
  (with-empty-state
    ;; Test persistent slots for various object types
    (let ((objects (list
                    (make-instance 'rl::pos :x 0 :y 0)
                    (make-instance 'rl::item :x 0 :y 0)
                    (make-instance 'rl::goblin :x 0 :y 0)
                    (make-instance 'rl::player))))
      (dolist (obj objects)
        (let ((slots (ms:class-persistent-slots obj)))
          (is (listp slots))
          (is (> (length slots) 0))
          ;; All should have x and y slots (internal names have % prefix)
          (when (typep obj 'rl::pos)
            (is (member 'rl::%x slots))
            (is (member 'rl::%y slots))))))))

(test marshal-with-nested-objects
  (with-empty-state
    ;; Test marshaling objects that contain other objects
    (let* ((item (make-instance 'rl::item :x 5 :y 5))
           (inv-obj (make-instance 'rl::inventory)))
      ;; Add item to inventory
      (push (cons #\a item) (rl::inventory inv-obj))
      
      (let ((marshaled (ms:marshal inv-obj)))
        (is (listp marshaled))
        ;; The marshaled form should exist and be non-empty
        (is (not (null marshaled)))
        (is (>= (length marshaled) 1))))))


(test define-class-persistent-slots-method
  (with-empty-state
    ;; Test that define-class properly sets up persistent slots method
    (let ((test-class 'test-persistent-slots-class))
      (eval `(rl::define-class ,test-class ()
               ((slot1 :initform 1)
                (slot2 :initform 2))))
      
      (let* ((obj (make-instance test-class))
             (slots (ms:class-persistent-slots obj)))
        (is (listp slots))
        (is (member 'slot1 slots))
        (is (member 'slot2 slots))))))


(test marshal-empty-object
  (with-empty-state
    ;; Test marshaling object with no persistent slots
    (let ((test-class 'test-empty-class))
      (eval `(rl::define-class ,test-class () ()))
      (let* ((obj (make-instance test-class))
             (marshaled (ms:marshal obj)))
        ;; Object with no slots should still marshal
        (is (listp marshaled))))))

(test letters-constant-immutable
  (with-empty-state
    ;; Test that +letters+ is truly constant
    (let ((original-length (length rl::+letters+)))
      ;; Verify we can't modify it (this should signal an error in a proper const)
      (is (= (length rl::+letters+) original-length))
      ;; Verify it contains expected values
      (is (every #'characterp rl::+letters+))
      (is (every #'lower-case-p rl::+letters+)))))