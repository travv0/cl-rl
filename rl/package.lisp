;;;; package.lisp

(defpackage #:rl
  (:use #:cl #:dynamic-mixins)
  (:local-nicknames (:dungen :net.mfiano.lisp.dungen))
  (:import-from #:alexandria
                #:when-let
                #:when-let*
                #:if-let
                #:curry
                #:rcurry
                #:compose
                #:removef
                #:make-keyword
                #:lastcar
                #:random-elt
                #:assoc-value)
  (:import-from #:serapeum
                #:defconst
                #:op
                #:do-hash-table)
  (:export #:tick
           #:initialize
           #:error-condition
           #:visible-keyword
           #:states
           #:+letters+))

(defpackage #:astar
  (:use #:cl)
  (:export #:find-shortest-path
           #:node-path))
