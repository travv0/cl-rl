;; Test display width issue

(require :asdf)
(load #P"~/quicklisp/setup.lisp")

(push (merge-pathnames "rl/" *default-pathname-defaults*) asdf:*central-registry*)
(push (merge-pathnames "rl-curses/" *default-pathname-defaults*) asdf:*central-registry*)
(push (merge-pathnames "travv0.utils/" *default-pathname-defaults*) asdf:*central-registry*)

(ql:quickload '(:rl :rl-curses) :silent t)

;; Test chunk range
(rl::initialize)

(format t "Stage size: ~dx~d~%" rl::*stage-width* rl::*stage-height*)
(format t "Chunk size: ~dx~d~%" rl::*chunk-width* rl::*chunk-height*)
(format t "Player position: ~a~%" rl::*player*)

(multiple-value-bind (min-x min-y max-x max-y)
    (rl::chunk-range-to-show)
  (format t "Chunk range to show: (~d,~d) to (~d,~d)~%" min-x min-y max-x max-y)
  (format t "Total width: ~d, Total height: ~d~%" (- max-x min-x) (- max-y min-y)))

;; Test what objects are being dumped
(let ((state (rl::dump-state)))
  (let ((objects (getf (second state) :objects)))
    (format t "Number of objects to display: ~d~%" (length objects))
    (when (> (length objects) 0)
      (format t "First few objects:~%")
      (loop for obj in (subseq objects 0 (min 5 (length objects)))
            do (format t "  ~a at (~d,~d)~%" 
                      (getf obj :name)
                      (getf obj :x)
                      (getf obj :y))))))

(sb-ext:exit :code 0)