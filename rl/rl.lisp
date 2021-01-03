;;;; rl.lisp

(in-package #:rl)

(defparameter *log* '())

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

(defparameter *name-cache* (make-hash-table))

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
  (push obj *game-objects*)
  (when (typep obj 'pos)
    (push obj (aref *pos-cache* (x obj) (y obj))))
  obj)

(defun clear-objects ()
  (setf *game-objects* '())
  (setf *pos-cache* (make-array (list *stage-width* *stage-height*)
                                :element-type 'list
                                :initial-element '())))

(defun clear-position (pos)
  (loop for obj in (get-objects-at-pos pos) do
    (removef *game-objects* obj))
  (setf (aref *pos-cache* (x pos) (y pos)) '()))

(defmethod dump-object ((obj visible) &optional attributes)
  (list :name (make-keyword (class-name (primary-class-of-mixin obj)))
        :display-name (display-name obj)
        :x (x obj)
        :y (y obj)
        :attributes attributes))

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
                            when (can-see (first objs))
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
  (when (ecase *state*
          (:play
           (ecase action
             ((nil))
             (:wait t)
             (:move-left (setf (dx *player*) -1) t)
             (:move-up (setf (dy *player*) -1) t)
             (:move-right (setf (dx *player*) 1) t)
             (:move-down (setf (dy *player*) 1) t)
             (:move-up-left (setf (dx *player*) -1 (dy *player*) -1) t)
             (:move-up-right (setf (dx *player*) 1 (dy *player*) -1) t)
             (:move-down-left (setf (dx *player*) -1 (dy *player*) 1) t)
             (:move-down-right (setf (dx *player*) 1 (dy *player*) 1) t)
             (:run-left
              (ensure-mix *player* 'running)
              (setf (dx *player*) -1)
              t)
             (:run-up
              (ensure-mix *player* 'running)
              (setf (dy *player*) -1)
              t)
             (:run-right
              (ensure-mix *player* 'running)
              (setf (dx *player*) 1)
              t)
             (:run-down
              (ensure-mix *player* 'running)
              (setf (dy *player*) 1)
              t)
             (:run-up-left
              (ensure-mix *player* 'running)
              (setf (dx *player*) -1 (dy *player*) -1)
              t)
             (:run-up-right
              (ensure-mix *player* 'running)
              (setf (dx *player*) 1 (dy *player*) -1)
              t)
             (:run-down-left
              (ensure-mix *player* 'running)
              (setf (dx *player*) -1 (dy *player*) 1)
              t)
             (:run-down-right
              (ensure-mix *player* 'running)
              (setf (dx *player*) 1 (dy *player*) 1)
              t)
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
               (mapc (lambda (obj) (setf (can-see obj) nil)) *game-objects*)

               (dolist (obj *game-objects*)
                 (unless (typep obj 'deleted)
                   (if (cooling-down-p obj)
                       (cool-down obj)
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
