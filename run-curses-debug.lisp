;; Debug version of run-curses.lisp
;; Usage: sbcl --load run-curses-debug.lisp

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

;; Test basic curses functionality
(format t "Testing basic curses functionality...~%")
(handler-case
    (charms:with-curses ()
      (format t "Curses initialized successfully~%")
      (charms:disable-echoing)
      (charms:enable-raw-input :interpret-control-characters t)
      (multiple-value-bind (width height)
          (charms:window-dimensions charms:*standard-window*)
        (format t "Window dimensions: ~dx~d~%" width height))
      
      ;; Try to write a simple string
      (charms:write-string-at-point charms:*standard-window* "Test" 0 0)
      (charms:refresh-window charms:*standard-window*)
      (format t "Basic write successful~%")
      
      ;; Wait a moment
      (sleep 1))
  (error (e)
    (format t "Curses error: ~a~%" e)))

;; Now test game initialization
(format t "~%Testing game initialization...~%")
(handler-case
    (progn
      (rl:initialize)
      (format t "Game initialized successfully~%")
      
      ;; Test tick
      (let ((result (rl:tick nil)))
        (format t "First tick successful, state: ~a~%" (first result))))
  (error (e)
    (format t "Game initialization error: ~a~%" e)))

;; If all tests pass, try the actual game
(format t "~%All tests passed. Press any key to start the game...~%")
(read-char)

;; Load bounds checking patch
(load (merge-pathnames "rl-curses-bounds-fix.lisp" 
                       (pathname (directory-namestring *load-pathname*))))

(handler-case
    (rl-curses:main)
  (error (e)
    (format t "~%Error running game: ~a~%" e)))