;;;; package.lisp

(defpackage #:rl
  (:use #:cl #:box.ecs)
  (:import-from #:alexandria
                #:when-let
                #:curry)
  (:import-from #:serapeum
                #:op)
  (:export #:*display-function*
           #:tick
           #:initialize
           #:error-condition))
