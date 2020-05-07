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

(defmethod display-name (obj)
  (flet ((format-name (class)
           (str:replace-all "-" " " (string-downcase (class-name class)))))
    (let ((obj-name (format-name (primary-class-of-mixin obj)))
          (obj-modifiers (mapcar (op (format-name _))
                                 (get-modifiers obj))))
      (format nil "~{~a ~}~a" obj-modifiers obj-name))))

(define-condition quit-condition () ())

(defun add-object (obj)
  (push obj *game-objects*)
  (when (typep obj 'pos)
    (push obj (aref *pos-cache* (x obj) (y obj)))))

(defmethod dump-object ((obj visible) &optional attributes)
  (list :name (make-keyword (class-name (primary-class-of-mixin obj)))
        :x (x obj)
        :y (y obj)
        :attributes attributes))

(defmethod dump-object ((door door) &optional attributes)
  (call-next-method door (concatenate 'list
                                      (list :open (not (typep door 'solid)))
                                      attributes)))

(defmethod dump-object ((memory memory) &optional attributes)
  (call-next-method memory (concatenate 'list
                                        (list :memory-of (memory-of memory))
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

(defvar *turn* 1)

(defun dump-state ()
  (list :player (dump-object *player*)
        :log *log*
        :turn *turn*
        :objects (loop with result = '()
                       for y below (array-dimension *pos-cache* 1)
                       finally (return result)
                       do (loop for x below (array-dimension *pos-cache* 0)
                                for obj = (get-object-at-pos (pos x y))
                                when obj
                                  do (push (dump-object obj) result)))))

(defun initialize ()
  (setf *turn* 1)
  (setf *log* '())
  (setf *game-objects* '())
  (setf *pos-cache* (make-array (list *stage-width* *stage-height*)
                                :element-type 'list
                                :initial-element '()))
  (init-cells *stage-width* *stage-height*)
  (init-floor *stage-width* *stage-height*)
  (let ((pos (random-pos)))
    (setf *player* (make-instance 'player :x (x pos) :y (y pos))))
  (add-object *player*)
  (loop repeat 5 do
    (let ((pos (random-pos)))
      (add-object (make-instance (if (zerop (random 2)) 'goblin 'goblin-fighter)
                                 :x (x pos)
                                 :y (y pos)))))
  (loop repeat 5 do
    (let ((pos (random-pos)))
      (add-object (make-instance 'rat
                                 :x (x pos)
                                 :y (y pos)))))
  (loop repeat 10 do
    (let ((pos (random-pos)))
      (add-object (make-instance 'warrior
                                 :x (x pos)
                                 :y (y pos)))))
  (loop repeat 5 do
    (let ((pos (random-pos)))
      (add-object (make-instance 'potion
                                 :x (x pos)
                                 :y (y pos)))))
  (loop repeat 5 do
    (let ((pos (random-pos)))
      (add-object (make-instance 'sword
                                 :x (x pos)
                                 :y (y pos))))))

(defun tick (action)
  (delete-from-mix *player* 'running)
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

  (loop do (when (not (plusp (health *player*)))
             (initialize))

           (unless (or (cooling-down-p *player*) (attacking-p *player*))
             (mapc (op (delete-from-mix _ 'can-see)) *game-objects*))

           (dolist (obj *game-objects*)
             (cond ((attacking-p obj) (progress-attack obj))
                   ((cooling-down-p obj) (cool-down obj))
                   (t (update obj))))

           (setf *game-objects*
                 (loop for obj in *game-objects*
                       if (typep obj 'deleted)
                         do (delete-from-mix obj 'deleted)
                            (removef (aref *pos-cache* (x obj) (y obj)) obj)
                       else collect obj))

           (incf *turn*)
        while (or (plusp (cooldown *player*)) (typep *player* 'attacking)))

  (dump-state))
