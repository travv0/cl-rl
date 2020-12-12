;;;; package.lisp

(defpackage #:rl-curses
  (:use #:cl #:alexandria)
  (:import-from #:travv0.utils
                #:desfun)
  (:import-from #:serapeum
                #:@
                #:ecase-of)
  (:export #:dev #:main))
