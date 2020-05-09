(in-package #:rl)

(defvar *player*)
(defun player () *player*)

(defclass player (moveable visible solid can-see health stamina humanoid)
  ((%health :initform 100)
   (%stamina :initform 100)
   (%equip-left-arm :initform (make-instance 'kite-shield))))

(defmethod update ((player player))
  (loop for y below (array-dimension *pos-cache* 1) do
    (loop for x below (array-dimension *pos-cache* 0) do
      (when (or (zerop x)
                (= x (1- *stage-width*))
                (zerop y)
                (= y (1- *stage-height*)))
        (block pos-loop
          (loop with hit-opaque = nil
                for pos in (rest (get-line player (pos x y)))
                do (loop for obj in (get-objects-at-pos pos)
                         do (ensure-mix obj 'can-see)
                            (when (typep obj 'opaque)
                              (setf hit-opaque t)))
                   (when-let ((obj (get-object-at-pos pos)))
                     (unless (or (eq obj player) (typep obj 'moveable))
                       (replace-memory obj)))
                   (when hit-opaque
                     (return-from pos-loop))))))))

(defmethod update :after ((player player))
  (with-accessors ((x x) (y y)) player
    (flet ((visible-pos (check-x check-y)
             (and (not (typep (get-object-at-pos (pos check-x check-y)) 'opaque))
                  (some (op (and (typep _1 'can-see) (not (typep _1 'memory))))
                        (get-objects-at-pos (pos check-x check-y))))))
      (loop for obj in *game-objects*
            when (and (typep obj 'opaque) (not (typep obj 'can-see)))
              do (when (or (and (>= x (x obj))
                                (>= y (y obj))
                                (or (visible-pos (1+ (x obj)) (y obj))
                                    (visible-pos (x obj) (1+ (y obj)))))
                           (and (<= x (x obj))
                                (>= y (y obj))
                                (or (visible-pos (1- (x obj)) (y obj))
                                    (visible-pos (x obj) (1+ (y obj)))))
                           (and (<= x (x obj))
                                (<= y (y obj))
                                (or (visible-pos (1- (x obj)) (y obj))
                                    (visible-pos (x obj) (1- (y obj)))))
                           (and (>= x (x obj))
                                (<= y (y obj))
                                (or (visible-pos (1+ (x obj)) (y obj))
                                    (visible-pos (x obj) (1- (y obj))))))
                   (replace-memory obj)
                   (ensure-mix obj 'can-see))))))
