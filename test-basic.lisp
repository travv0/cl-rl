#!/usr/bin/env sbcl --script

(require :asdf)

;; Try to load quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (format t "Loading quicklisp...~%")
    (load quicklisp-init)))

;; Add current directory to ASDF search path
(push (merge-pathnames "rl/" (pathname (directory-namestring *load-pathname*))) 
      asdf:*central-registry*)

(format t "Testing basic system loading...~%")

;; First, try to just find the system
(handler-case
    (progn
      (format t "Finding rl system...~%")
      (asdf:find-system :rl)
      (format t "Found rl system~%")
      
      (format t "Finding rl/tests system...~%")
      (asdf:find-system :rl/tests)
      (format t "Found rl/tests system~%"))
  (error (e)
    (format t "Error finding systems: ~A~%" e)
    (sb-ext:exit :code 1)))

(format t "Systems found successfully!~%")