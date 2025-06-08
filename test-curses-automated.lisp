;; Automated test for curses interface issues
;; This simulates the game loop without actually running curses

(require :asdf)
(load #P"~/quicklisp/setup.lisp")

(push (merge-pathnames "rl/" *default-pathname-defaults*) asdf:*central-registry*)
(push (merge-pathnames "rl-curses/" *default-pathname-defaults*) asdf:*central-registry*)
(push (merge-pathnames "travv0.utils/" *default-pathname-defaults*) asdf:*central-registry*)

(ql:quickload '(:rl :rl-curses) :silent t)

(format t "Testing game simulation...~%")

;; Initialize the game
(rl:initialize)

;; Simulate several game ticks with different movements
(let ((movements '(:move-left :move-right :move-up :move-down 
                   :move-up-left :move-up-right :move-down-left :move-down-right
                   nil nil nil))) ; Some waits too
  
  (format t "Testing ~d movements...~%" (length movements))
  
  (loop for movement in movements
        for i from 1
        do (handler-case
               (progn
                 (format t "~%Turn ~d: Testing ~a... " i movement)
                 (let ((result (rl:tick movement)))
                   (format t "OK")
                   ;; Also test that we can extract display data
                   (when (eq (first result) :play)
                     (let ((data (second result)))
                       (format t " (Player at ~d,~d)" 
                               (getf (getf data :player) :x)
                               (getf (getf data :player) :y))))))
             (error (e)
               (format t "ERROR!~%")
               (format t "  Error type: ~a~%" (type-of e))
               (format t "  Error message: ~a~%" e)
               (when (search "Invalid function name" (format nil "~a" e))
                 (format t "  VARIABLE SHADOWING BUG DETECTED!~%")
                 (format t "  Stack trace:~%")
                 (sb-debug:print-backtrace :stream *standard-output* :count 10))
               (sb-ext:exit :code 1)))))

(format t "~%~%All movements completed successfully!~%")
(sb-ext:exit :code 0)