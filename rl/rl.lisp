;;;; rl.lisp

(in-package #:rl)

(defparameter *log* '())

(defvar *game-state-lock* (bt:make-lock "game-state-lock")
  "Lock for thread-safe access to *game-objects* and *pos-cache*")

(defun write-to-log (format-control &rest format-args)
  (push (apply #'format nil format-control format-args) *log*))

(defgeneric update (object)
  (:documentation "What the object should do each tick."))

(defmethod update (obj))

(defun primary-class-of-mixin (obj)
  (let ((class (typecase obj
                 (class obj)
                 (t (class-of obj)))))
    (typecase class
      (mixin-class (lastcar (c2mop:class-direct-superclasses class)))
      (t (first (c2mop:class-precedence-list class))))))

(defparameter *name-cache* (make-hash-table :test 'eq :weakness :key)
  "Weak hash table for caching display names. Keys are automatically removed when objects are GC'd.")

(defmethod display-name (obj)
  (if-let ((name (gethash obj *name-cache*)))
    name
    (flet ((format-name (class)
             (substitute #\space #\- (string-downcase (class-name class)))))
      (let ((obj-name (format-name (primary-class-of-mixin obj)))
            (obj-modifiers (mapcar (op (format-name _))
                                   (get-modifiers obj))))
        (setf (gethash obj *name-cache*)
              (format nil "~{~a ~}~a" obj-modifiers obj-name))))))

(define-condition quit-condition () ())

(defun add-object (obj)
  (safe-push-to-game-objects obj)
  (when (typep obj 'pos)
    (safe-push-to-pos-cache obj (x obj) (y obj)))
  obj)

(defun clear-objects ()
  (safe-clear-game-state))

(defun clear-position (pos)
  (loop for obj in (get-objects-at-pos pos) do
    (safe-remove-from-game-objects obj))
  (safe-set-pos-cache-at (x pos) (y pos) '()))

(defmethod dump-object ((obj visible) &optional attributes)
  (list :name (make-keyword (class-name (primary-class-of-mixin obj)))
        :display-name (display-name obj)
        :x (x obj)
        :y (y obj)
        :attributes attributes
        :actions (actions obj)))

(defmethod dump-object ((door door) &optional attributes)
  (call-next-method door (concatenate 'list
                                      (list :open (not (typep door 'solid)))
                                      attributes)))

(defmethod dump-object ((player player) &optional attributes)
  (call-next-method player (concatenate 'list
                                        (list :health (health *player*)
                                              :max-health (max-health *player*)
                                              :previous-health (previous-health *player*)
                                              :stamina (stamina *player*)
                                              :max-stamina (max-stamina *player*)
                                              :previous-stamina (previous-stamina *player*))
                                        attributes)))

(defmethod dump-object ((obj rechargeable) &optional attributes)
  (call-next-method obj (concatenate 'list
                                     (list :charges (current-charges obj)
                                           :max-charges (max-charges obj))
                                     attributes)))

(defvar *turn* 1)
(defvar *seed*)
(defvar *state* :play)

(defun dump-state ()
  "dump the game state to a format for the front ends to use for rendering"
  (ecase *state*
    (:play
     (list :player (dump-object *player*)
           :log *log*
           :turn *turn*
           :seed *seed*
           :objects
           (multiple-value-bind (start-x start-y end-x end-y) (chunk-range-to-show)
             (loop with result = '()
                   for y from start-y below end-y
                   finally (return result)
                   do (loop for x from start-x below end-x
                            for objs = (get-objects-at-pos (pos x y))
                            when (some #'can-see objs)
                              do (setf result (append (reverse (mapcar #'dump-object objs))
                                                      result)))))))
    (:inventory
     (list :inventory (loop for (char . item) in (inventory *player*)
                            collect (cons char (dump-object
                                                item
                                                (list :equipped
                                                      (when-let ((arm (find-if (op (eq (equipped-weapon _) item))
                                                                               (arms *player*))))
                                                        (arm-name arm))))))))))

(defun spawn-pos ()
  (find-if (op (typep _ 'spawn)) *game-objects*))

(defun initialize (&optional seed)
  (sb-ext:gc :full t)
  (setf *turn* 1)
  (setf *seed* (or seed (random 10000000)))
  (setf *random-state* (sb-ext:seed-random-state *seed*))
  (setf *log* '())
  (clear-objects)
  (init-floor *stage-width* *stage-height* *seed*)
  (let ((pos (spawn-pos)))
    (setf *player* (make-instance 'player :x (x pos) :y (y pos))))
  (loop repeat 10 do
    (let ((pos (random-pos)))
      (add-object (make-instance 'goblin
                                 :x (x pos)
                                 :y (y pos)))))
  ;; (loop repeat 5 do
  ;;   (let ((pos (random-pos)))
  ;;     (add-object (make-instance 'warrior
  ;;                                :x (x pos)
  ;;                                :y (y pos)))))
  (loop repeat 5 do
    (let ((pos (random-pos)))
      (add-object (make-instance 'sword
                                 :x (x pos)
                                 :y (y pos)))))
  (loop repeat 5 do
    (let ((pos (random-pos)))
      (add-object (make-instance 'kite-shield
                                 :x (x pos)
                                 :y (y pos)))))

  (save-world)
  (unload-world)
  (ensure-chunks-loaded (chunks-to-show))

  (update *player*)
  (update-can-see *player*))

(defun tick (action)
  (delete-from-mix *player* 'running)

  (dolist (obj (cons *player* *game-objects*))
    (when (typep obj 'visible)
      (setf (actions obj) '())))

  (when (ecase *state*
          (:play
           (ecase action
             ((nil))
             (:wait t)
             (:move-left (process-movement -1 nil))
             (:move-up (process-movement nil -1))
             (:move-right (process-movement 1 nil))
             (:move-down (process-movement nil 1))
             (:move-up-left (process-movement -1 -1))
             (:move-up-right (process-movement 1 -1))
             (:move-down-left (process-movement -1 1))
             (:move-down-right (process-movement 1 1))
             (:run-left (process-movement -1 nil t))
             (:run-up (process-movement nil -1 t))
             (:run-right (process-movement 1 nil t))
             (:run-down (process-movement nil 1 t))
             (:run-up-left (process-movement -1 -1 t))
             (:run-up-right (process-movement 1 -1 t))
             (:run-down-left (process-movement -1 1 t))
             (:run-down-right (process-movement 1 1 t))
             (:open-inventory (setf *state* :inventory) nil)
             (:reset (initialize) nil)
             (:quit (error 'quit-condition))))

          (:inventory
           (case action
             (:close-inventory (setf *state* :play) nil)
             (#.*inventory-chars*
              (let ((item (assoc-value (inventory *player*) action)))
                (apply-item item *player*)
                (typep item 'useable))))))

    (loop do (when (not (plusp (health *player*)))
               (initialize))

             (let ((*game-objects* (cons *player* *game-objects*)))
               (dolist (obj *game-objects*)
                 (setf (can-see obj) nil))

               (dolist (obj *game-objects*)
                 (unless (typep obj 'deleted)
                   (cool-down obj)
                   (unless (cooling-down-p obj)
                     (update obj)))))

             (setf *game-objects*
                   (loop for obj in *game-objects*
                         if (typep obj 'deleted)
                           do (delete-from-mix obj 'deleted)
                              (removef (aref *pos-cache* (x obj) (y obj)) obj)
                         else collect obj))

             (when (zerop (cooldown *player*))
               (update-can-see *player*)
               (update-chunks))

             (incf *turn*)
          while (plusp (cooldown *player*))))

  (list *state* (dump-state)))

(defun process-movement (dx dy &optional running-p)
  "Process player movement with optional running mode"
  (when running-p
    (ensure-mix *player* 'running))
  (when dx
    (setf (dx *player*) dx))
  (when dy
    (setf (dy *player*) dy))
  t)

(defun get-all-subclasses (class)
  (let* ((class (etypecase class
                  (symbol (find-class class))
                  (class class)))
         (subclasses (c2mop:class-direct-subclasses class)))
    (when subclasses
      (remove-if (op (typep _ 'mixin-class))
                 (append subclasses (mapcan #'get-all-subclasses subclasses))))))

(defun get-visible-keywords ()
  (mapcar (compose #'make-keyword #'class-name)
          (get-all-subclasses 'visible)))

(deftype visible-keyword ()
  `(member ,@(get-visible-keywords)))

(deftype states ()
  `(member :play :inventory))
