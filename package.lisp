;;;; package.lisp

(defpackage #:rl
  (:use #:cl #:box.ecs #:dynamic-mixins)
  (:import-from #:alexandria
                #:when-let
                #:curry
                #:removef)
  (:import-from #:serapeum
                #:op
                #:do-hash-table)
  (:export #:*display-function*
           #:tick
           #:initialize
           #:error-condition))
