;; Run RL in curses mode
;; Usage: sbcl --load run-curses.lisp

(require :asdf)

;; Load quicklisp
(load #P"~/quicklisp/setup.lisp")

;; Add paths to ASDF
(push (merge-pathnames "rl/" (pathname (directory-namestring *load-pathname*))) 
      asdf:*central-registry*)
(push (merge-pathnames "rl-curses/" (pathname (directory-namestring *load-pathname*)))
      asdf:*central-registry*)
(push (merge-pathnames "travv0.utils/" (pathname (directory-namestring *load-pathname*)))
      asdf:*central-registry*)

;; Load the system
(format t "Loading RL curses interface...~%")
(ql:quickload :rl-curses :silent t)

;; Run the game with error handling
(format t "Starting game...~%")

(handler-case
    (rl-curses:main)
  (error (e)
    (format t "~%Error running game: ~a~%" e)
    (format t "~%Common issues:~%")
    (format t "1. Terminal window too small (needs at least 80x24)~%")
    (format t "2. Terminal doesn't support curses properly~%")
    (format t "3. Missing terminal capabilities~%~%")
    (format t "Try resizing your terminal window and running again.~%")))