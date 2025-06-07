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