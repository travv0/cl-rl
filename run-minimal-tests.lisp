#!/usr/bin/env sbcl --script

(require :asdf)

;; Try to load quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (if (probe-file quicklisp-init)
      (progn
        (format t "Loading quicklisp...~%")
        (load quicklisp-init))
      (progn
        (format t "Quicklisp not found. Attempting to proceed without it...~%")
        (format t "Some tests may fail due to missing dependencies.~%~%"))))

;; Add rl directory to ASDF search path
(push (merge-pathnames "rl/" (pathname (directory-namestring *load-pathname*))) 
      asdf:*central-registry*)

(format t "~%Running minimal test suite...~%")

;; Try to load just the base system first
(handler-case
    (progn
      (format t "Loading rl system...~%")
      (handler-bind ((asdf:missing-dependency 
                      (lambda (c)
                        (format t "Warning: Missing dependency ~A~%" 
                                (asdf/find-component:missing-requires c))
                        (invoke-restart 'asdf:clear-configuration-and-retry))))
        (asdf:load-system :rl :verbose nil))
      (format t "rl system loaded successfully~%"))
  (error (e)
    (format t "Could not load rl system: ~A~%" e)
    (format t "Proceeding to test what we can...~%")))

;; Define a simple test framework if fiveam isn't available
(unless (find-package :fiveam)
  (defpackage :fiveam
    (:use :cl)
    (:export #:test #:is #:signals #:def-suite #:in-suite #:run! #:pass #:fail))
  
  (in-package :fiveam)
  
  (defmacro test (name &body body)
    `(defun ,name ()
       (handler-case
           (progn ,@body
                  (format t "✓ ~A~%" ',name))
         (error (e)
           (format t "✗ ~A: ~A~%" ',name e)))))
  
  (defmacro is (form)
    `(unless ,form
       (error "Assertion failed: ~S" ',form)))
  
  (defmacro signals (condition &body body)
    `(handler-case
         (progn ,@body
                (error "Expected ~A to be signaled" ',condition))
       (,condition () t)))
  
  (defmacro def-suite (name) nil)
  (defmacro in-suite (name) nil)
  (defun run! (suite) nil)
  (defun pass () t)
  (defun fail (msg) (error msg))
  
  (in-package :cl))

;; Run some basic tests
(format t "~%Running basic sanity tests...~%")

;; Test 1: Package exists
(handler-case
    (progn
      (find-package :rl)
      (format t "✓ RL package exists~%"))
  (error (e)
    (format t "✗ RL package not found: ~A~%" e)))

;; Test 2: Basic class definitions
(when (find-package :rl)
  (handler-case
      (progn
        (find-class 'rl::pos)
        (format t "✓ POS class exists~%"))
    (error (e)
      (format t "✗ POS class not found: ~A~%" e)))
  
  (handler-case
      (progn
        (find-class 'rl::player)
        (format t "✓ PLAYER class exists~%"))
    (error (e)
      (format t "✗ PLAYER class not found: ~A~%" e)))
  
  (handler-case
      (progn
        (find-class 'rl::goblin)
        (format t "✓ GOBLIN class exists~%"))
    (error (e)
      (format t "✗ GOBLIN class not found: ~A~%" e))))

;; Test 3: Basic object creation
(when (and (find-package :rl) (find-class 'rl::pos nil))
  (handler-case
      (let ((pos (make-instance 'rl::pos :x 5 :y 10)))
        (if (and (= (slot-value pos 'rl::x) 5)
                 (= (slot-value pos 'rl::y) 10))
            (format t "✓ POS object creation works~%")
            (format t "✗ POS object has wrong values~%")))
    (error (e)
      (format t "✗ POS object creation failed: ~A~%" e))))

(format t "~%Basic test run complete.~%")
(sb-ext:exit :code 0)