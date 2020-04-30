;;;; package.lisp

(defpackage #:rl
  (:use #:cl #:box.ecs)
  (:import-from #:alexandria
                #:when-let)
  (:export #:*display-function*
           #:tick
           #:initialize
           #:error-condition))
