(in-package #:rl)

(defvar *player*)

(define-class player (alive visible solid humanoid)
  ((%strength :initform 5)
   (%dexterity :initform 5)
   (%endurance :initform 5)
   (%vitality :initform 5)
   (%resistance :initform 5)
   (%intelligence :initform 5)
   (%faith :initform 5)
   (%inventory :initform (make-inventory (make-instance 'health-potion :charges 5)))
   (%view-distance :initform 40 :accessor view-distance))
  (:documentation "the class of the player character"))

(defun update-can-see (from)
  "set `can-see' to t for any objects visible from `from' object's current position"
  (multiple-value-bind (start-x start-y end-x end-y) (chunk-range-to-show)
    (loop for y from start-y below end-y do
      (loop for x from start-x below end-x do
        (when (or (= x start-x)
                  (= x (1- end-x))
                  (= y start-y)
                  (= y (1- end-y)))
          (setf (can-see from) t)
          (block pos-loop
            (loop for distance from (view-distance from) downto 0
                  for pos in (rest (get-line from (pos x y)))
                  do (when-let ((obj (get-object-at-pos pos)))
                       (unless (plusp distance)
                         (return-from pos-loop))
                       (unless (can-see obj)
                         (setf (can-see obj) t))
                       (when (typep obj 'opaque)
                         (return-from pos-loop)))))))))
  ;; if an object is opaque but not can-see, set can-see to t if it
  ;; has a visible object next to it so that there aren't random
  ;; invisible positions in walls
  (with-accessors ((x x) (y y)) from
    (flet ((visible-pos (check-x check-y)
             (and (not (typep (get-visible-object-at-pos (pos check-x check-y)) 'opaque))
                  (some (op (can-see _))
                        (get-objects-at-pos (pos check-x check-y))))))
      (loop for obj in *game-objects*
            when (and (typep obj 'opaque) (not (can-see obj)))
              do (when (or (and (>= x (x obj))
                                (>= y (y obj))
                                (and (visible-pos (1+ (x obj)) (y obj))
                                     (visible-pos (x obj) (1+ (y obj)))))
                           (and (<= x (x obj))
                                (>= y (y obj))
                                (and (visible-pos (1- (x obj)) (y obj))
                                     (visible-pos (x obj) (1+ (y obj)))))
                           (and (<= x (x obj))
                                (<= y (y obj))
                                (and (visible-pos (1- (x obj)) (y obj))
                                     (visible-pos (x obj) (1- (y obj)))))
                           (and (>= x (x obj))
                                (<= y (y obj))
                                (and (visible-pos (1+ (x obj)) (y obj))
                                     (visible-pos (x obj) (1- (y obj))))))
                   (setf (can-see obj) t))))))
