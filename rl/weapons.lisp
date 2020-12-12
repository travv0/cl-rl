(in-package #:rl)

(define-class weapon (item damage stamina-use useable)
  ((%weapon-cooldown :initarg :weapon-cooldown
                     :initform (error "weapon-cooldown must be initialized")
                     :accessor weapon-cooldown)
   (%weapon-windup :accessor weapon-windup)
   (%weapon-strength-scale :initform (error "weapon-strength-scale must be initialized")
                           :accessor weapon-strength-scale
                           :type (integer 1 5))
   (%weapon-dexterity-scale :initform (error "weapon-dexterity-scale must be initialized")
                            :accessor weapon-dexterity-scale
                            :type (integer 1 5))))

(defmethod initialize-instance :after ((weapon weapon) &key)
  (unless (slot-boundp weapon '%stamina-use)
    (setf (stamina-use weapon) (+ (floor (damage weapon) 2) 5)))
  (unless (slot-boundp weapon '%weapon-windup)
    (setf (weapon-windup weapon) (floor (weapon-cooldown weapon) 2))))

(defmethod apply-item ((weapon weapon) (obj arms))
  ;; first try to unequip weapon
  (loop for arm in (arms obj)
        when (eq (equipped-weapon arm) weapon)
          do (setf (equipped-weapon arm) nil)
             (write-to-log "~a unequipped ~a from ~a"
                           (display-name obj)
                           (display-name weapon)
                           (arm-name arm))
             (return-from apply-item))
  ;; if weapon isn't equipped, equip it to first open arm
  (loop for arm in (arms obj)
        unless (equipped-weapon arm)
          do (setf (equipped-weapon arm) weapon)
             (write-to-log "~a equipped ~a to ~a"
                           (display-name obj)
                           (display-name weapon)
                           (arm-name arm))
             (return)
        finally (write-to-log "~a has no open slots to equip ~a"
                              (display-name obj)
                              (display-name weapon))))

(define-class dagger (weapon)
  ((%damage :initform 15)
   (%weapon-cooldown :initform 2)
   (%weapon-strength-scale :initform 1)
   (%weapon-dexterity-scale :initform 3)
   (%use-cooldown :initform 5)))

(define-class sword (weapon)
  ((%damage :initform 25)
   (%weapon-cooldown :initform 4)
   (%weapon-strength-scale :initform 3)
   (%weapon-dexterity-scale :initform 2)
   (%use-cooldown :initform 10)))

(define-class shield (weapon)
  ((%damage-reduction :initarg :damage-reduction
                      :initform (error "damage-reduction must be initialized")
                      :accessor damage-reduction)
   (%stability :initarg :stability
               :initform (error "stability must be initialized")
               :accessor stability)))

(define-class kite-shield (shield)
  ((%damage :initform 15)
   (%weapon-cooldown :initform 5)
   (%weapon-strength-scale :initform 2)
   (%weapon-dexterity-scale :initform 1)
   (%damage-reduction :initform 0.95)
   (%stability :initform 0.5)
   (%use-cooldown :initform 10)))
