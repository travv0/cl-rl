(require :asdf)
(require :sb-cover)

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

;; Enable coverage collection
(declaim (optimize sb-cover:store-coverage-data))

;; Load systems with coverage
(asdf:load-system :rl :verbose nil :force t)
(asdf:load-system :rl/tests :verbose nil :force t)

;; Run tests
(format t "~%Running tests with coverage enabled...~%~%")
(rl/tests:run-tests)

;; Generate coverage report
(format t "~%~%Generating coverage report...~%")
(sb-cover:report "/tmp/cl-rl-coverage/")
(format t "Coverage report generated in /tmp/cl-rl-coverage/~%")
(format t "Open /tmp/cl-rl-coverage/cover-index.html to view the report~%")

(sb-ext:exit :code 0)