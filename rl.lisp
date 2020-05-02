;;;; rl.lisp

(in-package #:rl)

(defclass pos ()
  ((%x :initarg :x :initform 0 :reader x)
   (%y :initarg :y :initform 0 :reader y)))

(defparameter *pos-cache* (serapeum:dict))

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
  ((%dx :initarg :dx :initform 0 :accessor dx)
   (%dy :initarg :dy :initform 0 :accessor dy)
   (%friction :initarg :friction :initform 1 :accessor friction)))

(defclass visible (pos)
  ((%char :initarg :char :accessor display-char)
   (%foreground-color :initarg :fg-color :accessor foreground-color :initform :white)
   (%background-color :initarg :bg-color :accessor background-color :initform :black)
   (%bold-color :initarg :bold :accessor bold-color :initform t)))

(defclass can-see ()
  ())

(defclass memory (visible)
  ((%foreground-color :initform :blue)
   (%background-color :initform :black)
   (%bold-color :initform nil)))

(defclass solid () ())

(defclass inventory ()
  ((%inventory :initarg :inventory :initform '() :accessor inventory)))

(defclass player (moveable visible solid inventory)
  ((%char :initform #\@)))

(defclass item (visible)
  ((%char :initform #\?)))

(defclass weapon (item)
  ((%char :initform #\))))

(defclass sword (weapon)
  ((%foreground-color :initform :magenta)))

(defclass deleted ()
  ())

(defclass running ()
  ())

(defclass opaque ()
  ())

(defclass cell (visible)
  ((%char :initform #\.)))

(defgeneric update (object)
  (:documentation "What the object should do each tick."))

(defmethod update (obj))

(defun get-objects-at-pos (pos)
  (gethash (list (x pos) (y pos)) *pos-cache*))

(defun get-object-at-pos (pos)
  (find-if #'should-display (gethash (list (x pos) (y pos)) *pos-cache*)))

(defun replace-memory (obj)
  (let ((cache (gethash (list (x obj) (y obj)) *pos-cache*)))
    (setf (gethash (list (x obj) (y obj)) *pos-cache*)
          (remove-if (op (typep _ 'memory)) cache)
          (gethash (list (x obj) (y obj)) *pos-cache*)
          (append (gethash (list (x obj) (y obj)) *pos-cache*)
                  (list (make-instance 'memory :char (display-char obj) :x (x obj) :y (y obj)))))))

(defmethod update ((player player))
  (do-hash-table (key objs *pos-cache*)
    (declare (ignore objs))
    (destructuring-bind (x y) key
      (when (or (zerop x)
                (= x (1- *stage-width*))
                (zerop y)
                (= y (1- *stage-height*)))
        (block pos-loop
          (loop for pos in (get-line player (pos x y)) do
            (when-let ((obj (get-object-at-pos pos)))
              (unless (eq obj player)
                (replace-memory obj)))
            (loop for obj in (get-objects-at-pos pos) do
              (ensure-mix obj 'can-see)
              (when (typep obj 'opaque)
                (return-from pos-loop))))))))
  (call-next-method))

(defmethod update :after ((player player))
  (with-accessors ((x x) (y y)) player
    (flet ((visible-pos (check-x check-y)
             (and (not (typep (get-object-at-pos (pos check-x check-y)) 'wall))
                  (some (op (and (typep _1 'can-see) (not (typep _1 'memory))))
                        (get-objects-at-pos (pos check-x check-y))))))
      (loop for obj in *game-objects*
            when (and (typep obj 'wall) (not (typep obj 'can-see)))
              do (when (or (and (>= x (x obj))
                                (>= y (y obj))
                                (or (visible-pos (1+ (x obj)) (y obj))
                                    (visible-pos (x obj) (1+ (y obj)))))
                           (and (<= x (x obj))
                                (>= y (y obj))
                                (or (visible-pos (1- (x obj)) (y obj))
                                    (visible-pos (x obj) (1+ (y obj)))))
                           (and (<= x (x obj))
                                (<= y (y obj))
                                (or (visible-pos (1- (x obj)) (y obj))
                                    (visible-pos (x obj) (1- (y obj)))))
                           (and (>= x (x obj))
                                (<= y (y obj))
                                (or (visible-pos (1+ (x obj)) (y obj))
                                    (visible-pos (x obj) (1- (y obj))))))
                   (replace-memory obj)
                   (ensure-mix obj 'can-see))))))

(defmethod update-pos ((obj pos) new-x new-y)
  (with-accessors ((x x) (y y)) obj
    (removef (gethash (list x y) *pos-cache*) obj)
    (push obj (gethash (list new-x new-y) *pos-cache*))
    (setf (slot-value obj '%x) new-x
          (slot-value obj '%y) new-y)))

(defmethod update :before ((obj moveable))
  (with-accessors ((x x) (y y)
                   (dx dx) (dy dy)
                   (friction friction))
      obj
    (loop with collisions = (sort (check-collisions obj) #'< :key (op (distance obj (cdr _))))
          for collision in collisions do
            (destructuring-bind (other-obj . last-pos) collision
              (when (and (typep obj 'solid) other-obj last-pos (typep other-obj 'solid))
                (setf dx 0 dy 0)
                (update-pos obj (x last-pos) (y last-pos))
                (return))
              (when (and other-obj (typep other-obj 'item) (typep obj 'inventory))
                (push other-obj (inventory obj))
                (ensure-mix other-obj 'deleted)
                (when (typep obj 'running)
                  (update-pos obj (x other-obj) (y other-obj))
                  (setf dx 0 dy 0)
                  (delete-from-mix obj 'running)
                  (return)))))
    (update-pos obj (+ x (round dx)) (+ y (round dy)))
    (setf dx (- dx (* dx friction))
          dy (- dy (* dy friction)))))

(defmethod get-line ((start pos) (end pos))
  (let* ((x1 (x start))
         (y1 (y start))
         (x2 (x end))
         (y2 (y end))
         (dist-x (abs (- x1 x2)))
         (dist-y (abs (- y1 y2)))
         (steep (> dist-y dist-x)))
    (when steep
      (psetf x1 y1
             y1 x1
             x2 y2
             y2 x2))
    (when (> x1 x2)
      (psetf x1 x2
             x2 x1
             y1 y2
             y2 y1))
    (let* ((delta-x (- x2 x1))
           (delta-y (abs (- y1 y2)))
           (error (floor delta-x 2))
           (y-step (if (< y1 y2) 1 -1))
           (y y1)
           result)
      (loop for x from x1 to x2 do
        (push (if steep
                  (pos y x)
                  (pos x y))
              result)
        (decf error delta-y)
        (when (< error 0)
          (incf y y-step)
          (incf error delta-x)))
      (if (and (= (x start) (x (first result)))
               (= (y start) (y (first result))))
          result
          (reverse result)))))

(defmethod check-collisions ((obj moveable))
  (loop with collisions = '()
        for other-obj in *game-objects*
        when (not (eq obj other-obj)) do
          (loop with previous-step = obj
                for step in (get-line obj (pos (+ (x obj) (dx obj))
                                               (+ (y obj) (dy obj))))
                when (and (= (x step) (x other-obj))
                          (= (y step) (y other-obj)))
                  do (push (cons other-obj previous-step) collisions)
                do (setf previous-step step))
        finally (return collisions)))

(defclass wall (visible solid opaque)
  ((%foreground-color :initform :yellow)
   (%background-color :initform :black)
   (%bold-color :initform nil)))

(defvar *display-function*
  (lambda (x y char fg-color bg-color bold)
    (declare (ignore x y char fg-color bg-color bold))
    (error "*display-function* must be set to a function that draws entities"))
  "function that displays entites")

(defmethod display ((obj visible))
  (funcall *display-function*
           (x obj)
           (y obj)
           (display-char obj)
           (foreground-color obj)
           (background-color obj)
           (bold-color obj)))

(defvar *player*)

(defparameter *game-objects* '())

(define-condition quit-condition () ())

(defun add-object (obj)
  (push obj *game-objects*)
  (when (typep obj 'pos)
    (push obj (gethash (list (x obj) (y obj)) *pos-cache*))))

(defun initialize ()
  (setf *game-objects* '())
  (setf *pos-cache* (serapeum:dict))
  (init-cells *stage-width* *stage-height*)
  (init-floor *stage-width* *stage-height*)
  (setf *player* (make-instance 'player :x 40 :y 10))
  (add-object *player*)
  (add-object (make-instance 'sword :x 5 :y 5)))

(defun tick (display-function action)
  (let ((run-speed 100))
    (case action
      ((nil))
      (:move-left (setf (dx *player*) -1))
      (:move-up (setf (dy *player*) -1))
      (:move-right (setf (dx *player*) 1))
      (:move-down (setf (dy *player*) 1))
      (:move-up-left (setf (dx *player*) -1 (dy *player*) -1))
      (:move-up-right (setf (dx *player*) 1 (dy *player*) -1))
      (:move-down-left (setf (dx *player*) -1 (dy *player*) 1))
      (:move-down-right (setf (dx *player*) 1 (dy *player*) 1))
      (:run-left
       (ensure-mix *player* 'running)
       (setf (dx *player*) (- run-speed)))
      (:run-up
       (ensure-mix *player* 'running)
       (setf (dy *player*) (- run-speed)))
      (:run-right
       (ensure-mix *player* 'running)
       (setf (dx *player*) run-speed))
      (:run-down
       (ensure-mix *player* 'running)
       (setf (dy *player*) run-speed))
      (:run-up-left
       (ensure-mix *player* 'running)
       (setf (dx *player*) (- run-speed) (dy *player*) (- run-speed)))
      (:run-up-right
       (ensure-mix *player* 'running)
       (setf (dx *player*) run-speed (dy *player*) (- run-speed)))
      (:run-down-left
       (ensure-mix *player* 'running)
       (setf (dx *player*) (- run-speed) (dy *player*) run-speed))
      (:run-down-right
       (ensure-mix *player* 'running)
       (setf (dx *player*) run-speed (dy *player*) run-speed))
      (:quit (error 'quit-condition))
      (t (format t "Unknown key: ~a (~d)~%" (code-char action) action))))

  (dolist (obj *game-objects*)
    (update obj))

  (setf *game-objects*
        (loop for obj in *game-objects*
              if (typep obj 'deleted)
                do (delete-from-mix obj 'deleted)
                   (removef (gethash (list (x obj) (y obj)) *pos-cache*) obj)
              else collect obj))

  (let ((*display-function* display-function))
    (do-hash-table (key objs *pos-cache*)
      (declare (ignore key))
      (when-let ((obj (find-if #'should-display objs)))
        (display obj))))

  (mapc (op (delete-from-mix _ 'can-see)) *game-objects*))

(defun should-display (obj)
  (or (typep obj 'can-see) (typep obj 'memory)))

(defparameter *stage-width* 79)
(defparameter *stage-height* 23)

(defun init-cells (width height)
  (loop for y below height do
    (loop for x below width do
      (let ((cell (make-instance 'cell :x x :y y)))
        (add-object cell)))))

(defun init-floor (width height)
  (let ((stage (dungen:make-stage :density 0.5
                                  :wild-factor 1
                                  :room-extent 9
                                  :door-rate 0.1
                                  :width width
                                  :height height)))
    (loop for y from (1- (dungen:stage-height stage)) downto 0 do
      (loop for x below (dungen:stage-width stage)
            for cell = (dungen:get-cell stage x y)
            do (cond ((dungen:has-feature-p cell :wall)
                      (add-object (make-wall x y))))))))

(defun make-wall (x y)
  (make-instance 'wall :x x :y y :char #\#))

(initialize)
