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
                :dynamic-mixins :marshal :fiveam :closer-mop
                :defpackage-plus)
              :silent t)

;; Load systems
(asdf:load-system :rl :verbose nil)
(asdf:load-system :rl/tests :verbose nil)

;; Run tests directly
(rl/tests:run-tests)

(sb-ext:exit :code 0)