#!/usr/bin/env sbcl --script

(require :asdf)

;; Load quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (if (probe-file quicklisp-init)
      (load quicklisp-init)
      (progn
        (format t "Quicklisp not found. Please install it first.~%")
        (sb-ext:exit :code 1))))

;; Add paths to ASDF
(push (merge-pathnames "rl/" (pathname (directory-namestring *load-pathname*))) 
      asdf:*central-registry*)
(push (merge-pathnames "travv0.utils/" (pathname (directory-namestring *load-pathname*)))
      asdf:*central-registry*)

(format t "~%Loading dependencies...~%")

;; Load dependencies quietly
(handler-case
    (progn
      (ql:quickload '(:alexandria :serapeum :str :black-tie 
                      :dynamic-mixins :marshal :fiveam :closer-mop)
                    :silent t)
      (format t "Dependencies loaded successfully~%"))
  (error (e)
    (format t "Error loading dependencies: ~A~%" e)))

(format t "~%Loading rl system...~%")

;; Try to load the system
(handler-case
    (progn
      (asdf:load-system :rl :verbose nil)
      (format t "RL system loaded successfully~%"))
  (error (e)
    (format t "Error loading rl system: ~A~%" e)
    (format t "~%Continuing with tests anyway...~%")))

(format t "~%Loading test system...~%")

;; Load test system
(handler-case
    (progn
      (asdf:load-system :rl/tests :verbose nil)
      (format t "Test system loaded successfully~%"))
  (error (e)
    (format t "Error loading test system: ~A~%" e)
    (sb-ext:exit :code 1)))

(format t "~%Running tests...~%~%")

;; Run the tests
(handler-case
    (let ((results (rl/tests:run-tests)))
      (format t "~%~%Tests completed!~%")
      (force-output))
  (error (e)
    (format t "~%Error running tests: ~A~%" e)
    (sb-ext:exit :code 1)))

(sb-ext:exit :code 0)