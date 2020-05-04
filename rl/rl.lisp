;;;; rl.lisp

(in-package #:rl)

(defparameter *log* '())

(defun write-to-log (format-control &rest format-args)
  (push (apply #'format nil format-control format-args) *log*))

(defgeneric update (object)
  (:documentation "What the object should do each tick."))

(defmethod update (obj))

(defmethod display-name (obj)
  (flet ((format-name (class)
           (str:replace-all "-" " " (string-downcase (class-name class)))))
    (let ((obj-name (format-name
                     (typecase (class-of obj)
                       (mixin-class (lastcar (c2mop:class-direct-superclasses (class-of obj))))
                       (t (first (c2mop:class-precedence-list (class-of obj)))))))
          (obj-modifiers (mapcar (op (format-name _))
                                 (get-modifiers obj))))
      (format nil "~{~a ~}~a" obj-modifiers obj-name))))

(define-condition quit-condition () ())

(defun add-object (obj)
  (push obj *game-objects*)
  (when (typep obj 'pos)
    (push obj (aref *pos-cache* (x obj) (y obj)))))

(defun dump-state ()
  (list :health (health *player*)
        :max-health (max-health *player*)
        :stamina (stamina *player*)
        :max-stamina (max-stamina *player*)
        :log *log*
        :objects (loop with result = '()
                       for y below (array-dimension *pos-cache* 1)
                       finally (return result)
                       do (loop for x below (array-dimension *pos-cache* 0)
                                for obj = (get-object-at-pos (pos x y))
                                when obj
                                  do (push obj result)))))

(defun initialize ()
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
  (loop repeat 50 do
    (let ((pos (random-pos)))
      (add-object (make-instance (if (zerop (random 2)) 'goblin 'goblin-fighter)
                                 :x (x pos)
                                 :y (y pos)))))
  (let ((pos (random-pos)))
    (add-object (make-instance 'rat
                               :x (x pos)
                               :y (y pos))))
  (loop repeat 5 do
    (let ((pos (random-pos)))
      (add-object (make-instance 'potion
                                 :x (x pos)
                                 :y (y pos))))))

(defun tick (action)
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

           (unless (cooling-down-p *player*)
             (mapc (op (delete-from-mix _ 'can-see)) *game-objects*))

           (dolist (obj *game-objects*)
             (cond ((cooling-down-p obj) (cool-down obj))
                   (t (update obj))))

           (setf *game-objects*
                 (loop for obj in *game-objects*
                       if (typep obj 'deleted)
                         do (delete-from-mix obj 'deleted)
                            (removef (aref *pos-cache* (x obj) (y obj)) obj)
                       else collect obj))
        while (or (plusp (cooldown *player*)) (typep *player* 'running)))

  (dump-state))
