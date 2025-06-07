(defpackage :rl/tests
  (:use #:cl #:rl #:fiveam #:alexandria)
  (:export #:run-tests))

(in-package #:rl/tests)

(def-suite rl)

(defun run-tests ()
  (run! 'rl))

(defmacro with-initialized-state (&body body)
  `(let (rl::*turn*
         rl::*seed*
         rl::*log*
         rl::*game-objects*
         rl::*pos-cache*
         rl::*player*)
     (initialize 12345)
     ,@body))

(defmacro with-empty-state (&body body)
  `(let ((rl::*turn* 1)
         (rl::*seed* 12345)
         (rl::*log* '())
         (rl::*game-objects* '())
         (rl::*pos-cache* (make-array (list rl::*stage-width* rl::*stage-height*)
                                      :element-type 'list
                                      :initial-element '()))
         (rl::*player* nil))
     ,@body))
