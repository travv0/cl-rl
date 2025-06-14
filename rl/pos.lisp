(in-package #:rl)

(define-class pos ()
  ((%x :initarg :x :initform 0 :reader x)
   (%y :initarg :y :initform 0 :reader y))
  (:documentation "represents a position as a 2d coordinate. anything
  with a position should inherit from this class"))

(defmethod print-object ((p pos) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~d ~d" (x p) (y p))))

(defun pos (x y)
  (make-instance 'pos :x x :y y))

(defmethod distance ((p1 pos) (p2 pos))
  (declare (optimize speed))
  (sqrt (+ (expt (- (x p2) (x p1)) 2)
           (expt (- (y p2) (y p1)) 2))))

(defmethod add ((p1 pos) (p2 pos))
  (make-instance 'pos :x (+ (x p1) (x p2))
                      :y (+ (y p1) (y p2))))

(defmethod sub ((p1 pos) (p2 pos))
  (make-instance 'pos :x (- (x p1) (x p2))
                      :y (- (y p1) (y p2))))

(defmethod mult ((p1 pos) b)
  (make-instance 'pos :x (* (x p1) b)
                      :y (* (y p1) b)))

(defmethod abs-val ((p pos))
  (make-instance 'pos :x (abs (x p))
                      :y (abs (y p))))

(defmethod pos-equal ((p1 pos) (p2 pos))
  (and (= (x p1) (x p2))
       (= (y p1) (y p2))))

(defun to-pos (pair)
  "converts a 2-element list or a dotted pair to a `pos'"
  (typecase (cdr pair)
    (cons (pos (first pair) (second pair)))
    (t (pos (car pair) (cdr pair)))))

(defmethod can-see-p ((origin pos) (target pos))
  "returns non-nil if `target' pos can be seen from `origin' pos"
  (loop for p in (rest (get-line origin target))
        do (loop for obj in (get-objects-at-pos p)
                 when (typep obj 'opaque)
                   do (return-from can-see-p))
        finally (return t)))

(defmethod get-line ((start pos) (end pos))
  "get the straight line between `start' and `end', represented as a
list of `pos's"
  (declare (optimize speed))
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

(defmethod in-bounds-p ((p pos))
  "Check if a position is within the stage bounds"
  (and (<= 0 (x p) (1- *stage-width*))
       (<= 0 (y p) (1- *stage-height*))))

(defun in-bounds-xy-p (x y)
  "Check if x,y coordinates are within the stage bounds"
  (and (<= 0 x (1- *stage-width*))
       (<= 0 y (1- *stage-height*))))

(defmethod update-pos ((obj pos) new-x new-y)
  "update the position of an object. use this instead of setting
object's x and y coordinates directly."
  (let ((new-x (clamp new-x 0 (1- *stage-width*)))
        (new-y (clamp new-y 0 (1- *stage-height*))))
    (with-accessors ((x x) (y y)) obj
      (safe-remove-from-pos-cache obj x y)
      (safe-push-to-pos-cache obj new-x new-y)
      (setf (slot-value obj '%x) new-x
            (slot-value obj '%y) new-y))))
