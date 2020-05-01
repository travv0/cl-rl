;;;; rl.lisp

(in-package #:rl)

(defclass pos ()
  ((x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y)))

(defmethod print-object ((p pos) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~d ~d" (x p) (y p))))

(defun pos (x y)
  (make-instance 'pos :x x :y y))

(defmethod distance ((p1 pos) (p2 pos))
  (sqrt (+ (expt (- (x p2) (x p1)) 2)
           (expt (- (y p2) (y p1)) 2))))

(defmethod sub ((p1 pos) (p2 pos))
  (make-instance 'pos :x (- (x p1) (x p2))
                      :y (- (y p1) (y p2))))

(defmethod mult ((p1 pos) b)
  (make-instance 'pos :x (* (x p1) b)
                      :y (* (y p1) b)))

(defmethod abs-val ((p pos))
  (make-instance 'pos :x (abs (x p))
                      :y (abs (y p))))

(defmethod same ((p1 pos) (p2 pos))
  (and (= (x p1) (x p2))
       (= (y p1) (y p2))))

(defclass moveable (pos)
  ((dx :initarg :dx :initform 0 :accessor dx)
   (dy :initarg :dy :initform 0 :accessor dy)
   (friction :initarg :friction :initform 1 :accessor friction)))

(defclass visible (pos)
  ((char :initarg :char :accessor display-char)))

(defclass solid () ())

(defclass inventory ()
  ((inventory :initarg :inventory :initform '() :accessor inventory)))

(defclass player (moveable visible solid inventory)
  ((char :initform #\@)))

(defclass item (visible)
  ((char :initform #\?)))

(defclass weapon (item)
  ((char :initform #\))))

(defclass sword (weapon)
  ())

(defgeneric update (object)
  (:documentation "What the object should do each tick."))

(defmethod update (obj))

(defmethod update :before ((obj moveable))
  (with-accessors ((x x) (y y)
                   (dx dx) (dy dy)
                   (friction friction))
      obj
    (loop with collisions = (sort (check-collisions obj) #'< :key (op (distance obj (cdr _))))
          for collision in collisions do
            (destructuring-bind (other-obj . last-pos) collision
              (when (and (typep obj 'solid) other-obj last-pos (typep other-obj 'solid))
                (setf dx 0 dy 0
                      x (x last-pos)
                      y (y last-pos))
                (return))
              (when (and other-obj (typep other-obj 'item) (typep obj 'inventory))
                (push other-obj (inventory obj))
                (setf *game-objects* (remove other-obj *game-objects*)))))
    (incf x (round dx))
    (incf y (round dy))
    (setf dx (- dx (* dx friction))
          dy (- dy (* dy friction)))))

(defmethod get-line ((start pos) (end pos) &key include-start include-end)
  (let* ((x-increment (if (> (x end) (x start)) 1 -1))
         (y-increment (if (> (y end) (y start)) 1 -1))
         (delta (abs-val (sub start end)))
         (err (- (x delta) (y delta)))
         (error-correct (mult delta 2))
         (current start)
         result)
    (loop when (or (and (same current start) include-start)
                   (and (same current end) include-end)
                   (and (not (same current start)) (not (same current end))))
            do (push current result)
          when (same current end)
            do (return-from get-line (reverse result))
          do (cond ((> err 0)
                    (setf current (pos (+ (x current) x-increment)
                                       (y current)))
                    (decf err (y error-correct)))
                   ((< err 0)
                    (setf current (pos (x current)
                                       (+ (y current) y-increment)))
                    (incf err (x error-correct)))
                   (t
                    (setf current (pos (+ (x current) x-increment)
                                       (+ (y current) y-increment))))))))

(defmethod check-collisions ((obj moveable))
  (loop with collisions = '()
        for other-obj in *game-objects*
        when (not (eq obj other-obj)) do
          (loop with previous-step = obj
                for step in (get-line obj (pos (+ (x obj) (dx obj))
                                               (+ (y obj) (dy obj)))
                                      :include-end t)
                when (and (= (x step) (x other-obj))
                          (= (y step) (y other-obj)))
                  do (push (cons other-obj previous-step) collisions)
                do (setf previous-step step))
        finally (return collisions)))

(defclass wall (visible solid) ())

(defvar *display-function*
  (lambda (x y char color)
    (declare (ignore x y char color))
    (error "*display-function* must be set to a function that draws entities"))
  "function that displays entites")

(defmethod display ((obj visible))
  (funcall *display-function*
           (x obj)
           (y obj)
           (display-char obj)
           nil))

(defparameter *player*
  (make-instance 'player :x 5 :y 1 :char #\@))

(defparameter *game-objects* '())

(define-condition quit-condition () ())

(defun initialize ()
  (setf *game-objects* '())
  (init-floor)
  (setf *player*
        (make-instance 'player :x 5 :y 1 :char #\@))
  (push *player* *game-objects*)
  (push (make-instance 'sword :x 5 :y 5) *game-objects*))

(defun tick (display-function key-code)
  (case key-code
    ((nil))
    (:move-left (setf (dx *player*) -1))
    (:move-up (setf (dy *player*) -1))
    (:move-right (setf (dx *player*) 1))
    (:move-down (setf (dy *player*) 1))
    (:move-up-left (setf (dx *player*) -1 (dy *player*) -1))
    (:move-up-right (setf (dx *player*) 1 (dy *player*) -1))
    (:move-down-left (setf (dx *player*) -1 (dy *player*) 1))
    (:move-down-right (setf (dx *player*) 1 (dy *player*) 1))
    (:quit (error 'quit-condition))
    (t (format t "Unknown key: ~a (~d)~%" (code-char key-code) key-code)))

  (dolist (obj *game-objects*)
    (update obj))
  (let ((*display-function* display-function))
    (dolist (obj *game-objects*)
      (display obj))))

(defun init-floor ()
  (let ((stage (dungen:make-stage :density 1
                                  :wild-factor 1
                                  :room-extent 9
                                  :door-rate 0.1
                                  :width 79
                                  :height 23)))
    (loop for y from (1- (dungen:stage-height stage)) downto 0 do
      (loop for x below (dungen:stage-width stage)
            for cell = (dungen:get-cell stage x y)
            do (cond ((dungen:has-feature-p cell :wall)
                      (push (make-wall x y) *game-objects*)))))))

(defun make-wall (x y)
  (make-instance 'wall :x x :y y :char #\#))
