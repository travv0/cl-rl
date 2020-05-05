;;;; package.lisp

(defpackage #:rl
  (:use #:cl #:box.ecs #:dynamic-mixins)
  (:import-from #:alexandria
                #:when-let
                #:when-let*
                #:curry
                #:rcurry
                #:removef
                #:make-keyword
                #:lastcar)
  (:import-from #:serapeum
                #:op
                #:do-hash-table)
  (:export #:tick
           #:initialize
           #:error-condition))

(defpackage #:astar
  (:use #:cl)
  (:export #:find-shortest-path
           #:node-path))
