(in-package #:rl/tests)
(in-suite rl)

;; Note: The utils.lisp file mainly contains marshal-related methods
;; which are complex to test without the full marshal system.
;; We'll focus on testing what we can.

(test utils-marshal-methods-exist
  (with-empty-state
    ;; Just verify the methods are defined
    (let ((obj (make-instance 'rl::pos)))
      ;; class-persistent-slots should return a list
      (is (listp (ms:class-persistent-slots obj))))))

(test define-class-macro
  (with-empty-state
    ;; The define-class macro is used throughout the codebase
    ;; We can test that classes defined with it work properly
    (let ((pos (make-instance 'rl::pos :x 5 :y 5)))
      (is (= (rl::x pos) 5))
      (is (= (rl::y pos) 5)))))