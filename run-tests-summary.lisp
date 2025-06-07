#!/usr/bin/env sbcl --script

(require :asdf)

;; Load quicklisp
(load #P"~/quicklisp/setup.lisp")

;; Add paths
(push (merge-pathnames "rl/" (pathname (directory-namestring *load-pathname*))) 
      asdf:*central-registry*)
(push (merge-pathnames "travv0.utils/" (pathname (directory-namestring *load-pathname*)))
      asdf:*central-registry*)

;; Load dependencies quietly  
(ql:quickload '(:alexandria :serapeum :str :black-tie 
                :dynamic-mixins :marshal :fiveam :closer-mop)
              :silent t)

;; Load systems
(asdf:load-system :rl :verbose nil)
(asdf:load-system :rl/tests :verbose nil)

;; Run tests with summary
(format t "~%Running test suite...~%~%")

(let ((results (rl/tests:run-tests)))
  (when results
    (format t "~%~%===== TEST SUMMARY =====~%")
    (format t "Total tests run: ~A~%" 
            (+ (length (it.bese.fiveam::passed results))
               (length (it.bese.fiveam::failed results))
               (length (it.bese.fiveam::skipped results))))
    (format t "Passed: ~A~%" (length (it.bese.fiveam::passed results)))
    (format t "Failed: ~A~%" (length (it.bese.fiveam::failed results)))
    (format t "Skipped: ~A~%" (length (it.bese.fiveam::skipped results)))
    
    (when (> (length (it.bese.fiveam::failed results)) 0)
      (format t "~%Failed tests:~%")
      (dolist (failure (it.bese.fiveam::failed results))
        (format t "  - ~A~%" (it.bese.fiveam::name (it.bese.fiveam::test-case failure)))))))

(sb-ext:exit :code 0)