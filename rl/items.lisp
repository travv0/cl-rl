(in-package #:rl)

(defclass item (visible)
  ())

(defclass rechargeable (useable)
  ((%max-charges :initarg :charges
                 :initform (error "max-charges is required")
                 :accessor max-charges)
   (%current-charges :accessor current-charges)))

(defclass useable ()
  ((%use-cooldown :initform (error "use-cooldown is required")
                  :accessor use-cooldown)))

(defmethod initialize-instance :after ((obj rechargeable) &key)
  (setf (current-charges obj) (max-charges obj)))

(defclass health-potion (rechargeable useable item)
  ((%regeneration-amount :initform 40 :reader regeneration-amount)
   (%max-charges :initform 5)
   (%use-cooldown :initform 15)))

(defmethod apply-item :around ((item rechargeable) obj)
  (cond ((plusp (current-charges item))
         (call-next-method)
         (decf (current-charges item)))
        (t (write-to-log "could not apply ~a - out of charges"
                         (display-name item))
           (setf *state* :play))))

(defmethod apply-item ((potion health-potion) (obj alive))
  (incf (health obj) (regeneration-amount potion)))

(defmethod apply-item :after ((potion health-potion) (obj alive))
  (write-to-log "~a drank a ~a" (display-name obj) (display-name potion)))

(defmethod apply-item :after ((item useable) (obj cooldown))
  (incf (cooldown obj) (use-cooldown item)))

(defmethod apply-item (item obj)
  (write-to-log "~a was unable to use ~a - don't know how"
                (display-name obj)
                (display-name item)))

(defmethod apply-item :after (item obj)
  (setf *state* :play))

(defparameter *inventory-chars*
  (append (loop for c from (char-code #\a) to (char-code #\z)
                collect (code-char c))
          (loop for c from (char-code #\A) to (char-code #\Z)
                collect (code-char c))))

(defmethod add-to-inventory ((item item) (inventory inventory))
  (loop for c in *inventory-chars*
        unless (assoc-value (inventory inventory) c)
          do (push (cons c item) (inventory inventory))
             (setf (inventory inventory) (sort (inventory inventory) #'char< :key #'car))
             (return (inventory inventory))))

(defun make-inventory (&rest items)
  (mapcar #'cons +letters+ items))
