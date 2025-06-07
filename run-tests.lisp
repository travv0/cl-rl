#!/usr/bin/env sbcl --script

(require :asdf)

;; Load quicklisp if available
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Add current directory to ASDF search path
(push (pathname (directory-namestring *load-pathname*)) asdf:*central-registry*)

;; Load the test system
(handler-case
    (progn
      (asdf:load-system :rl/tests)
      (funcall (intern "RUN-TESTS" :rl/tests)))
  (error (e)
    (format t "Error loading or running tests: ~A~%" e)
    (sb-ext:exit :code 1)))

(sb-ext:exit :code 0)