#!/usr/bin/env sbcl --script

(require :asdf)

;; Try to load quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (if (probe-file quicklisp-init)
      (progn
        (format t "Loading quicklisp...~%")
        (load quicklisp-init)
        
        ;; Ensure required dependencies are available
        (format t "~%Checking/installing dependencies...~%")
        (handler-case
            (progn
              (funcall (intern "QUICKLOAD" :ql) 
                       '(:alexandria :serapeum :str :black-tie 
                         :dynamic-mixins :marshal :fiveam :closer-mop)
                       :silent t))
          (error (e)
            (format t "Error loading dependencies via quicklisp: ~A~%" e)
            (format t "Please ensure quicklisp is installed and configured.~%")
            (sb-ext:exit :code 1))))
      (progn
        (format t "Quicklisp not found. Please install it first.~%")
        (sb-ext:exit :code 1))))

;; Add rl directory to ASDF search path
(push (merge-pathnames "rl/" (pathname (directory-namestring *load-pathname*))) 
      asdf:*central-registry*)

;; Also add travv0.utils if it exists locally
(let ((utils-path (merge-pathnames "travv0.utils/" 
                                   (pathname (directory-namestring *load-pathname*)))))
  (when (probe-file utils-path)
    (push utils-path asdf:*central-registry*)))

(format t "~%Loading rl system and tests...~%")

;; Load the test system
(handler-case
    (progn
      (asdf:load-system :rl/tests :verbose nil)
      (format t "~%Running tests...~%~%")
      (let ((results (funcall (intern "RUN-TESTS" :rl/tests))))
        (format t "~%Test run complete.~%")
        (if (and (find-class 'it.bese.fiveam::test-run-result nil)
                 (typep results 'it.bese.fiveam::test-run-result))
            (let ((failed (funcall (intern "FAILED" :it.bese.fiveam) results))
                  (passed (funcall (intern "PASSED" :it.bese.fiveam) results)))
              (format t "Passed: ~A~%" (length passed))
              (format t "Failed: ~A~%" (length failed))
              (when (> (length failed) 0)
                (sb-ext:exit :code 1)))
            (format t "Results: ~A~%" results))))
  (error (e)
    (format t "~%Error loading or running tests: ~A~%" e)
    (format t "~%Backtrace:~%")
    (sb-debug:print-backtrace :count 20)
    (sb-ext:exit :code 1)))

(sb-ext:exit :code 0)