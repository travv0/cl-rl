(in-package #:rl/tests)

;; Import the op macro from serapeum if available
(when (find-package :serapeum)
  (import 'serapeum:op :rl))

;; This file intentionally left minimal.
;; If tests fail due to missing functions, those functions should either:
;; 1. Be implemented in the main system, or
;; 2. The tests should be rewritten to not depend on non-existent functionality