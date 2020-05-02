;;;; rl.lisp

(in-package #:rl)

(defparameter *update-fns* (serapeum:dict))
(defmacro multiupdate ((classes obj-name) &body body)
  (flet ((make-typep (class)
           `(typep ,obj-name ',class)))
    `(setf (gethash ',(sort classes #'string<)
                    *update-fns*)
           (lambda (,obj-name)
             (when (and ,@(mapcar #'make-typep classes))
               ,@body)))))

(defun remove-update-fn (classes)
  (remhash (sort classes #'string<) *update-fns*))

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

(defclass cooldown ()
  ((%cooldown :initform 0 :accessor cooldown)))

(defclass moveable (pos cooldown)
  ((%dx :initarg :dx :initform 0 :accessor dx)
   (%dy :initarg :dy :initform 0 :accessor dy)
   (%friction :initarg :friction :initform 1 :accessor friction)
   (%move-cooldown :initarg :move-cooldown :initform 5 :accessor move-cooldown)))

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

(defclass health ()
  ((%health :initarg :health :initform 100 :accessor health)))

(defclass equip-weapon ()
  ())

(defclass right-arm (equip-weapon)
  ((%equip-right-arm :initarg :equip-right-arm :initform nil :accessor equip-right-arm)))

(defclass left-arm (equip-weapon)
  ((%equip-left-arm :initarg :equip-left-arm :initform nil :accessor equip-left-arm)))

(defclass player (moveable visible solid inventory can-see health right-arm left-arm)
  ((%char :initform #\@)))

(defclass enemy (solid moveable visible health)
  ((%enemy-state :initarg :enemy-state
                 :initform (if (zerop (random 2))
                               :wandering
                               :sleeping)
                 :accessor enemy-state)
   (%wandering-to :initform (pos (random *stage-width*) (random *stage-height*))
                  :accessor wandering-to)
   (%char :initform (error "enemies must have a char set"))))

(defclass goblin (enemy)
  ((%char :initform #\g)
   (%foreground-color :initform :green)
   (%bold-color :initform nil)))

(defclass wall (visible solid opaque)
  ((%char :initform #\#)
   (%foreground-color :initform :yellow)
   (%background-color :initform :black)
   (%bold-color :initform nil)))

(defclass door (visible)
  ((%char :initform #\+)
   (%foreground-color :initform :red)
   (%background-color :initform :black)
   (%bold-color :initform nil)))

(defclass item (visible)
  ((%char :initform #\?)))

(defclass damage ()
  ((%damage :initarg :damage :initform 30 :accessor damage)))

(defclass weapon (item)
  ((%char :initform #\))))

(defclass modifier ()
  ())

(defclass fire (modifier)
  ())

(defclass ice (modifier)
  ())

(defclass sword (weapon damage)
  ((%foreground-color :initform :magenta)))

(defclass deleted ()
  ())

(defclass running (pos)
  ())

(defclass opaque ()
  ())

(defclass cell (visible)
  ((%char :initform #\.)))

(defparameter *log* '())

(defun write-to-log (format-control &rest format-args)
  (push (apply #'format nil format-control format-args) *log*))

(defgeneric update (object)
  (:documentation "What the object should do each tick."))

(defmethod update (obj))

(defmethod update :before (obj)
  (maphash (op (funcall _2 obj)) *update-fns*))

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
          (loop with hit-opaque = nil
                for pos in (rest (get-line player (pos x y)))
                do (loop for obj in (get-objects-at-pos pos)
                         do (ensure-mix obj 'can-see)
                            (when (typep obj 'opaque)
                              (setf hit-opaque t)))
                   (when-let ((obj (get-object-at-pos pos)))
                     (unless (eq obj player)
                       (replace-memory obj)))
                   (when hit-opaque
                     (return-from pos-loop)))))))
  (call-next-method))

(defmethod update :after ((player player))
  (with-accessors ((x x) (y y)) player
    (flet ((visible-pos (check-x check-y)
             (and (not (typep (get-object-at-pos (pos check-x check-y)) 'opaque))
                  (some (op (and (typep _1 'can-see) (not (typep _1 'memory))))
                        (get-objects-at-pos (pos check-x check-y))))))
      (loop for obj in *game-objects*
            when (and (typep obj 'opaque) (not (typep obj 'can-see)))
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

(defmethod display-name (obj)
  (let ((obj-name (string-downcase
                   (class-name
                    (typecase (class-of obj)
                      (mixin-class (lastcar (c2mop:class-direct-superclasses (class-of obj))))
                      (t (first (c2mop:class-precedence-list (class-of obj))))))))
        (obj-modifiers (mapcar (op (string-downcase (class-name _)))
                               (remove-if (op (or (typep _1 'mixin-class)
                                                  (eq _1 (find-class 'modifier))
                                                  (not (c2mop:subtypep _1 (find-class 'modifier)))))
                                          (c2mop:class-precedence-list (class-of obj))))))
    (format nil "~{~a ~}~a" obj-modifiers obj-name)))

(defmethod collide ((obj pos) (moving-obj moveable)))

(defmethod collide :before ((door door) (moving-obj moveable))
  (when (typep door 'solid)
    (setf (display-char door) #\')
    (delete-from-mix door 'opaque 'solid)))

(defmethod collide :before ((obj item) (moving-obj inventory))
  (write-to-log "picked up ~:[something~;a~@[n~] ~0@*~a~]"
                (display-name obj)
                (member (char (display-name obj) 0) '(#\a #\e #\i #\o #\u)))
  (if (typep obj 'weapon)
      (setf (equip-right-arm moving-obj) obj)
      (push obj (inventory moving-obj)))
  (ensure-mix obj 'deleted))

(defparameter *melee-damage* 5)

(defmethod collide :before ((obj health) (arm right-arm))
  (let ((damage (if (equip-right-arm arm)
                    (damage (equip-right-arm arm))
                    *melee-damage*)))
    (write-to-log "~a attacked ~a for ~d damage"
                  (display-name arm)
                  (display-name obj)
                  damage)
    (decf (health obj) damage)
    (when (not (plusp (health obj)))
      (write-to-log "~a was defeated" (display-name obj))
      (ensure-mix obj 'deleted))))

(defmethod update :after ((obj cooldown))
  (when (plusp (cooldown obj))
    (decf (cooldown obj))))

(defun wall-p (x y dir)
  (ccase dir
    (:up (some (op (typep _ 'wall)) (get-objects-at-pos (pos x (1- y)))))
    (:down (some (op (typep _ 'wall)) (get-objects-at-pos (pos x (1+ y)))))
    (:left (some (op (typep _ 'wall)) (get-objects-at-pos (pos (1- x) y))))
    (:right (some (op (typep _ 'wall)) (get-objects-at-pos (pos (1+ x) y))))))

(defmethod wall-different-p ((obj pos) prev-x prev-y)
  (with-accessors ((x x) (y y)) obj
    (or (and (/= x prev-x) (or (not (eql (wall-p x y :up)
                                         (wall-p prev-x prev-y :up)))
                               (not (eql (wall-p x y :down)
                                         (wall-p prev-x prev-y :down)))))
        (and (/= y prev-y) (or (not (eql (wall-p x y :left)
                                         (wall-p prev-x prev-y :left)))
                               (not (eql (wall-p x y :right)
                                         (wall-p prev-x prev-y :right))))))))

(defun get-heuristic (start end)
  (+ (abs (- (car start) (car end)))
     (abs (- (cdr start) (cdr end)))))

(defun get-neighbors (pos)
  (mapcar (lambda (c) (cons (cons (x c) (y c)) 1))
          (remove-if (lambda (pos) (member-if (op (and (typep _1 'solid)
                                                       (not (typep _1 'door))))
                                              (get-objects-at-pos pos)))
                     (list
                      (pos (car pos) (1- (cdr pos)))
                      (pos (car pos) (1+ (cdr pos)))
                      (pos (1- (car pos)) (cdr pos))
                      (pos (1+ (car pos)) (cdr pos))))))

(defmethod find-path ((pos pos) (goal-pos pos))
  (let ((node (astar:find-shortest-path (cons (x pos) (y pos))
                                        (cons (x goal-pos) (y goal-pos))
                                        'get-neighbors
                                        'get-heuristic)))
    (when node (reverse (astar:node-path node)))))

(defmethod move-toward-goal ((obj moveable) (goal-pos pos))
  (let ((path (find-path obj goal-pos)))
    (when path
      (when-let* ((next-pos (second path))
                  (dx (- (car next-pos) (x obj)))
                  (dy (- (cdr next-pos) (y obj))))
        (setf (dx obj) dx
              (dy obj) dy)
        t))))

(defmethod update :before ((enemy enemy))
  (ccase (enemy-state enemy)
    (:chasing (move-toward-goal enemy *player*))
    (:wandering
     (move-toward-goal enemy (wandering-to enemy))
     (when (and (zerop (dx enemy)) (zerop (dy enemy)))
       (setf (wandering-to enemy) (pos (random *stage-width*)
                                       (random *stage-height*)))))
    (:sleeping nil)))

(defmethod update :before ((obj moveable))
  (with-accessors ((x x) (y y)
                   (dx dx) (dy dy)
                   (friction friction)
                   (move-cooldown move-cooldown)
                   (cooldown cooldown))
      obj
    (unless (plusp cooldown)
      (loop with collisions = (sort (check-collisions obj) #'< :key (op (distance obj (cdr _))))
            for collision in collisions do
              (destructuring-bind (other-obj . last-pos) collision
                (when (or (and (typep obj 'solid) (typep other-obj 'solid))
                          (and other-obj (typep obj 'running) (typep other-obj 'item)))
                  (setf dx 0 dy 0)
                  (update-pos obj (x last-pos) (y last-pos)))
                (unless (typep obj 'running)
                  (collide other-obj obj))
                (when (and (zerop dx) (zerop dy))
                  (delete-from-mix obj 'running)
                  (return))))
      (unless (and (zerop dx) (zerop dy))
        (let ((move-cooldown (if (typep obj 'running)
                                 (round (* move-cooldown 2/3))
                                 move-cooldown)))
          (setf cooldown move-cooldown)))
      (update-pos obj (+ x (round dx)) (+ y (round dy)))
      (when (and (typep obj 'running)
                 (wall-different-p obj (- x (round dx)) (- y (round dy))))
        (setf dx 0 dy 0)
        (delete-from-mix obj 'running))
      (unless (typep obj 'running)
        (setf dx (- dx (* dx friction))
              dy (- dy (* dy friction)))))))

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
        unless (eq obj other-obj)
          do (loop with previous-step = obj
                   for step in (get-line obj (pos (+ (x obj) (dx obj))
                                                  (+ (y obj) (dy obj))))
                   when (and (not (typep other-obj 'cell))
                             (not (typep other-obj 'memory))
                             (= (x step) (x other-obj))
                             (= (y step) (y other-obj)))
                     do (push (cons other-obj previous-step) collisions)
                   do (setf previous-step step))
        finally (return collisions)))

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
  (setf *log* '())
  (setf *game-objects* '())
  (setf *pos-cache* (serapeum:dict))
  (init-cells *stage-width* *stage-height*)
  (init-floor *stage-width* *stage-height*)
  (loop for x = (random *stage-width*)
        for y = (random *stage-height*)
        unless (member-if (op (typep _ 'solid)) (get-objects-at-pos (pos x y)))
          do (setf *player* (make-instance 'player :x x :y y))
             (return))
  (add-object *player*)
  (loop for x = (random *stage-width*)
        for y = (random *stage-height*)
        unless (member-if (op (typep _ 'solid)) (get-objects-at-pos (pos x y)))
          do (add-object (make-instance (mix 'fire 'ice 'sword) :x x :y y))
             (return))
  (loop repeat 5 do
    (loop for x = (random *stage-width*)
          for y = (random *stage-height*)
          unless (member-if (op (typep _ 'solid)) (get-objects-at-pos (pos x y)))
            do (add-object (make-instance 'goblin :x x :y y))
               (return))))

(defun tick (display-function action)
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
     (setf (dx *player*) -1))
    (:run-up
     (ensure-mix *player* 'running)
     (setf (dy *player*) -1))
    (:run-right
     (ensure-mix *player* 'running)
     (setf (dx *player*) 1))
    (:run-down
     (ensure-mix *player* 'running)
     (setf (dy *player*) 1))
    (:run-up-left
     (ensure-mix *player* 'running)
     (setf (dx *player*) -1 (dy *player*) -1))
    (:run-up-right
     (ensure-mix *player* 'running)
     (setf (dx *player*) 1 (dy *player*) -1))
    (:run-down-left
     (ensure-mix *player* 'running)
     (setf (dx *player*) -1 (dy *player*) 1))
    (:run-down-right
     (ensure-mix *player* 'running)
     (setf (dx *player*) 1 (dy *player*) 1))
    (:reveal-map (mapc #'replace-memory (reverse *game-objects*)))
    (:reset (initialize))
    (:quit (error 'quit-condition))
    (t (format t "Unknown key: ~a (~d)~%" (code-char action) action)))

  (loop do (dolist (obj *game-objects*)
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

           (mapc (op (delete-from-mix _ 'can-see)) *game-objects*)
        while (or (plusp (cooldown *player*)) (typep *player* 'running)))

  *log*)

(defun should-display (obj)
  (or (typep obj 'can-see) (typep obj 'memory)))

(defparameter *stage-width* 49)
(defparameter *stage-height* 23)

(defun init-cells (width height)
  (loop for y below height do
    (loop for x below width do
      (let ((cell (make-instance 'cell :x x :y y)))
        (add-object cell)))))

(defun init-floor (width height)
  (let* ((extent (- (ceiling (min (/ width 2) (/ height 2))) 2))
         (stage (dungen:make-stage :density 0.5
                                   :wild-factor 0.1
                                   :room-extent (if (evenp extent) (1- extent) extent)
                                   :door-rate 0.1
                                   :width width
                                   :height height)))
    (loop for y from (1- (dungen:stage-height stage)) downto 0 do
      (loop for x below (dungen:stage-width stage)
            for cell = (dungen:get-cell stage x y)
            do (cond ((dungen:has-feature-p cell :wall)
                      (add-object (make-wall x y)))
                     ((or (dungen:has-feature-p cell :door/vertical)
                          (dungen:has-feature-p cell :door/horizontal))
                      (add-object (make-door x y))))))))

(defun make-wall (x y)
  (make-instance 'wall :x x :y y))

(defun make-door (x y)
  (make-instance (mix 'door 'opaque 'solid) :x x :y y))
