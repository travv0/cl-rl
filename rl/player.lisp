(in-package #:rl)

(defvar *player*)
(defun player () *player*)

(defclass player (alive visible solid humanoid)
  ((%strength :initform 10)
   (%dexterity :initform 10)
   (%endurance :initform 100)
   (%vitality :initform 100)
   (%inventory :initform (list (cons #\a (make-instance 'health-potion :charges 5))
                               (cons #\b (make-instance 'sword))
                               (cons #\c (make-instance 'kite-shield))))
   (%view-distance :initform 40 :accessor view-distance)))

(defun update-can-see ()
  (loop for y below (array-dimension *pos-cache* 1) do
    (loop for x below (array-dimension *pos-cache* 0) do
      (when (or (zerop x)
                (= x (1- *stage-width*))
                (zerop y)
                (= y (1- *stage-height*)))
        (loop for obj in (get-objects-at-pos *player*) do
          (ensure-mix obj 'can-see))
        (block pos-loop
          (loop with hit-opaque = nil
                for distance from (view-distance *player*) downto 0
                for pos in (rest (get-line *player* (pos x y)))
                do (loop for obj in (get-objects-at-pos pos)
                         unless (plusp distance)
                           do (return-from pos-loop)
                         do (ensure-mix obj 'can-see)
                            (when (typep obj 'opaque)
                              (setf hit-opaque t)))
                   (when hit-opaque
                     (return-from pos-loop)))))))
  (with-accessors ((x x) (y y)) *player*
    (flet ((visible-pos (check-x check-y)
             (and (not (typep (get-visible-object-at-pos (pos check-x check-y)) 'opaque))
                  (some (op (typep _1 'can-see))
                        (get-objects-at-pos (pos check-x check-y))))))
      (loop for obj in *game-objects*
            when (and (typep obj 'opaque) (not (typep obj 'can-see)))
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
                   (ensure-mix obj 'can-see))))))
