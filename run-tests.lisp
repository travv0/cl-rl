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

(rl/tests:run-tests)

(sb-ext:exit :code 0)