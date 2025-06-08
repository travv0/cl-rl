(in-package #:rl)

(define-class cooldown ()
  ((%cooldown :initform 0 :accessor cooldown)))

(define-class moveable (pos cooldown)
  ((%dx :initarg :dx :initform 0 :accessor dx)
   (%dy :initarg :dy :initform 0 :accessor dy)
   (%friction :initarg :friction :initform 1 :accessor friction)
   (%move-cooldown :initarg :move-cooldown :initform 5 :accessor move-cooldown)))

(define-class visible (pos)
  ((%can-see :initform nil :accessor can-see)
   (%actions :initform '() :initarg :actions :accessor actions)))

(defgeneric can-see (obj)
  (:documentation "return non-nil if object can be seen from player's
  location. if object doesn't inherit from `visible', this should
  return nil and setting it should be a no-op"))

(defmethod can-see (obj)
  nil)

(defmethod (setf can-see) (value obj)
  nil)

(define-class solid () ())

(define-class inventory ()
  ((%inventory :initarg :inventory
               :initform '()
               :accessor inventory)))

(define-class resistance ()
  ((%resistance-to :initarg :resistance-to
                   :initform (error "resistence-to must be specified")
                   :accessor resistance-to)
   (%resistance-amount :initarg :resistance-amount
                       :initform 1.2
                       :accessor resistance-amount)))

(define-class damage ()
  ((%damage :initarg :damage :initform (error "damage must be set") :accessor damage)))

(define-class stamina-use ()
  ((%stamina-use :initarg :damage :accessor stamina-use)))

(define-class modifier ()
  ())

(define-class fire (modifier)
  ())

(define-class ice (modifier)
  ())

(define-class deleted ()
  ())

(define-class running (pos)
  ())

(define-class opaque ()
  ())

(define-class alive (moveable)
  ((%vitality :initform (error "vitality must be set") :accessor vitality)
   (%strength :initform (error "strength must be set") :accessor strength)
   (%dexterity :initform (error "dexterity must be set") :accessor dexterity)
   (%endurance :initform (error "endurance must be set") :accessor endurance)
   (%resistance :initform (error "resistance must be set") :accessor resistance)
   (%intelligence :initform (error "intelligence must be set") :accessor intelligence)
   (%faith :initform (error "faith must be set") :accessor faith)

   (%stamina :reader stamina)
   (%max-stamina :reader max-stamina)
   (%previous-stamina :accessor previous-stamina :initform 0)
   (%stamina-recharging :accessor stamina-recharging :initform nil)

   (%health :reader health)
   (%max-health :reader max-health)
   (%previous-health :accessor previous-health :initform 0)
   (%resistances :initform '() :initarg :resistances :accessor resistances)

   (%view-distance :initform (error "view-distance is requred") :initarg :view-distance :accessor view-distance)))

(defmethod calculate-max-health ((obj alive))
  (round (+ 90 (* 100 (/ (vitality obj) 20)))))

(defmethod calculate-max-stamina ((obj alive))
  (round (+ 90 (* 100 (/ (endurance obj) 50)))))

(defmethod initialize-instance :after ((obj alive) &key)
  (setf (slot-value obj '%max-health) (calculate-max-health obj)
        (slot-value obj '%max-stamina) (calculate-max-stamina obj)
        (slot-value obj '%health) (max-health obj)
        (slot-value obj '%stamina) (max-stamina obj)))

(defmethod (setf health) (new-value (obj alive))
  (when (< new-value (health obj))
    (setf (slot-value obj '%previous-health) (health obj)))
  (setf (slot-value obj '%health) (min (max-health obj) new-value)))

(defmethod (setf stamina) (new-value (obj alive))
  (when (< new-value (stamina obj))
    (setf (previous-stamina obj) (stamina obj)))
  (setf (slot-value obj '%stamina) (min (max-stamina obj) new-value)))

(define-class arm ()
  ((%equipped-weapon :initarg :equipped-weapon :initform nil :accessor equipped-weapon)
   (%arm-name :initarg :name :initform "" :accessor arm-name)))

(defmethod print-object ((arm arm) stream)
  (print-unreadable-object (arm stream :type t)
    (format stream "~s ~s" (arm-name arm) (equipped-weapon arm))))

(define-class arms ()
  ((%arms :initarg :arms :initform '() :accessor arms)))

(define-class humanoid (inventory arms)
  ((%arms :initform (list (make-instance 'arm :name "right hand")
                          (make-instance 'arm :name "left hand")))))

(defmethod initialize-instance :after ((obj humanoid) &key)
  (loop with arm-num = 0
        for (char . item) in (inventory obj)
        while (< arm-num (length (arms obj)))
        when (typep item 'weapon)
          do (setf (equipped-weapon (nth arm-num (arms obj))) item)
             (incf arm-num)))

(defun make-resistance (type &optional amount)
  (let ((resistance (make-instance 'resistance :resistance-to type)))
    (when amount
      (setf (resistance-amount resistance) amount))
    resistance))

(defparameter *running-stamina* 3)

(defmethod add-action (obj action))

(defmethod add-action ((obj visible) action)
  "add `action' to `obj''s action list. `action' should be a list
starting with a keyword and containing relevant info"
  (appendf (actions obj) (list action)))

(defmethod update :after ((obj moveable))
  (with-accessors ((x x) (y y)
                   (dx dx) (dy dy)
                   (friction friction)
                   (move-cooldown move-cooldown)
                   (cooldown cooldown))
      obj
    (unless (and (zerop dx) (zerop dy))
      (loop with collisions = (sort (check-collisions obj) #'< :key (op (distance obj (cdr _))))
            for collision in collisions do
              (destructuring-bind (other-obj . last-pos) collision
                (when (or (and (typep obj 'solid) (typep other-obj 'solid)))
                  (setf dx 0 dy 0)
                  (update-pos obj (x last-pos) (y last-pos)))
                (collide other-obj obj)
                (when (and (zerop dx) (zerop dy))
                  (delete-from-mix obj 'running)
                  (return))))

      (flet ((modify-for-running (cooldown)
               (if (typep obj 'running)
                   (round (* cooldown
                             (if (< (stamina obj) *running-stamina*)
                                 2
                                 2/3)))
                   cooldown))
             (modify-for-terrain (cooldown x y)
               (if-let ((pos (get-terrain-at-pos (pos x y))))
                 (round (* cooldown (cooldown-modifier pos)))
                 cooldown)))
        (unless (and (zerop dx) (zerop dy))
          (let ((move-cooldown (~> move-cooldown
                                   modify-for-running
                                   (modify-for-terrain x y))))
            (incf cooldown move-cooldown))))

      (let ((new-x (+ x (round dx)))
            (new-y (+ y (round dy))))
        (add-action obj (list :move :from (list (x obj) (y obj))
                                    :to (list new-x new-y)))
        (update-pos obj new-x new-y)))

    (when (and (typep obj 'alive) (typep obj 'running))
      (cond ((< (stamina obj) *running-stamina*)
             (delete-from-mix obj 'running))
            (t (decf (stamina obj) *running-stamina*))))

    (setf dx (- dx (* dx friction))
          dy (- dy (* dy friction)))))

(defmethod update :after ((obj alive))
  (when (> (previous-health obj) (health obj))
    (decf (previous-health obj) 5)))

(defmethod update :around ((obj alive))
  (let ((start-stamina (stamina obj)))
    (call-next-method)
    (unless (< (stamina obj) start-stamina)
      (setf (stamina obj) (min (+ (stamina obj) 1) (max-stamina obj))))))

(defmethod cool-down (obj))

(defmethod cool-down ((obj cooldown))
  (when (plusp (cooldown obj))
    (decf (cooldown obj))))

(defun cooling-down-p (obj)
  (and (typep obj 'cooldown) (plusp (cooldown obj))))

(let ((modifier-class (find-class 'modifier)))
  (defun get-modifiers (obj)
    (remove-if (op (or (typep _1 'mixin-class)
                       (eq _1 modifier-class)
                       (not (c2mop:subtypep _1 modifier-class))))
               (c2mop:class-precedence-list (class-of obj)))))
