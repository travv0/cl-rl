(in-package #:rl)

(defvar *pos-cache*)
(defvar *game-objects*)

(define-class spawn (pos)
  ())

(define-class wall (visible solid opaque)
  ())

(define-class grass (visible)
  ())

(define-class tree (visible solid opaque)
  ())

(define-class water (visible solid)
  ())

(define-class shallow-water (visible)
  ())

(define-class sand (visible)
  ())

(define-class door (visible)
  ())

(defun should-display (obj)
  (typep obj 'can-see))

(defparameter *stage-width* 100)
(defparameter *stage-height* 100)

(defun grass-area-noise (x y seed)
  (let ((noise (* (black-tie:perlin-noise-sf (/ x 250.0) (/ y 250.0) (/ seed 1.0)) 0.5)))
    (incf noise (* (black-tie:perlin-noise-sf (/ x 100.0) (/ y 100.0) (/ seed 1.0)) 0.25))
    (incf noise (* (black-tie:perlin-noise-sf (/ x 50.0) (/ y 50.0) (/ seed 1.0)) 0.125))
    (incf noise (* (black-tie:perlin-noise-sf (/ x 10.0) (/ y 10.0) (/ seed 1.0)) 0.03))))

(defun tree-noise (x y seed)
  (* (black-tie:perlin-noise-sf (/ x 0.9) (/ y 0.9) (/ seed 1.0)) 100))

(defun lava-area-noise (x y seed)
  (let ((noise (* (black-tie:perlin-noise-sf (/ x 5.0) (/ y 10.0) (/ seed 1.0)) 1)))
    (incf noise (* (black-tie:perlin-noise-sf (/ x 2.0) (/ y 2.0) (/ seed 1.0)) 0.5))))

(defun make-perlin-noise-seed (seed)
  (let* ((string-seed (write-to-string seed))
         (numerator (subseq string-seed 0 (- (length string-seed) 3)))
         (denominator (subseq string-seed (- (length string-seed) 3))))
    (handler-case (/ (parse-integer numerator) (parse-integer denominator))
      (arithmetic-error () (make-perlin-noise-seed (+ seed 9))))))

(defun make-secret-entrance ()
  (let ((tree (random-elt (remove-if-not (lambda (obj) (and (typep obj 'tree)
                                                            (< 0 (x obj) (1- *stage-width*))
                                                            (< 0 (y obj) (1- *stage-height*))))
                                         *game-objects*)))
        (directions (remove-duplicates
                     (tu:make-combos 2 '(-1 -1 0 1 1))
                     :test #'equal)))
    (clear-position tree)
    (add-object (make-water (x tree) (y tree) :shallow t))
    (loop for (dx dy) in (remove (random-elt directions)
                                 directions
                                 :test #'equal)
          do (add-object (make-tree (+ (x tree) dx)
                                    (+ (y tree) dy))))
    (pos (x tree) (y tree))))

(defun init-grass-area (width height seed)
  (flet ((retry ()
           (clear-objects)
           (init-grass-area width height (random 10000000))
           (return-from init-grass-area)))
    (let ((seed (if (integerp seed)
                    (make-perlin-noise-seed seed)
                    seed)))
      (loop for y from (1- height) downto 0 do
        (loop for x below width do
          (let* ((noise (grass-area-noise x y seed)))
            (cond ((< noise -0.12) (add-object (make-water x y)))
                  ((< noise -0.1) (add-object (make-water x y :shallow t)))
                  ((< noise -0.08) (add-object (make-sand x y)))
                  (t
                   (add-object (make-grass x y))
                   (when (> noise 0.1)
                     (let ((tree-noise (tree-noise x y seed)))
                       (when (> tree-noise 30)
                         (add-object (make-tree x y))))))))))

      (unless (and (> (count-if (op (typep _ 'water)) *game-objects*) 700)
                   (> (count-if (op (typep _ 'tree)) *game-objects*) 300))
        (retry))

      (let* ((secret-entrance (make-secret-entrance))
             (spawn (add-object (make-spawn))))
        (unless (find-path spawn secret-entrance)
          (retry))))))

(defun init-lava-area (x y width height seed)
  (let* ((noise (lava-area-noise x y seed)))
    (cond ((< noise -0.2) (add-object (make-water x y)))
          (t (let ((rock-noise (tree-noise x y seed)))
               (when (> rock-noise 40)
                 (add-object (make-tree x y))))))))

(defun init-floor (width height &optional (seed 0))
  (init-grass-area width height seed))

(defun make-wall (x y)
  (make-instance 'wall :x x :y y))

(defun make-grass (x y)
  (make-instance 'grass :x x :y y))

(defun make-tree (x y)
  (make-instance 'tree :x x :y y))

(defun make-water (x y &key shallow)
  (if shallow
      (make-instance 'shallow-water :x x :y y)
      (make-instance 'water :x x :y y)))

(defun make-sand (x y)
  (make-instance 'sand :x x :y y))

(defun make-spawn ()
  (let ((pos (random-pos)))
    (make-instance 'spawn :x (x pos) :y (y pos))))

(defun make-door (x y)
  (make-instance (mix 'opaque 'solid 'door) :x x :y y))

(defun get-objects-at-pos (pos)
  (when (and (<= 0 (x pos) (1- *stage-width*))
             (<= 0 (y pos) (1- *stage-height*)))
    (remove-if-not (op (typep _ 'visible))
                   (aref *pos-cache* (x pos) (y pos)))))

(defun get-visible-objects-at-pos (pos)
  (when (and (<= 0 (x pos) (1- *stage-width*))
             (<= 0 (y pos) (1- *stage-height*)))
    (remove-if-not #'should-display (aref *pos-cache* (x pos) (y pos)))))

(defun get-visible-object-at-pos (pos)
  (when (and (<= 0 (x pos) (1- *stage-width*))
             (<= 0 (y pos) (1- *stage-height*)))
    (find-if #'should-display (aref *pos-cache* (x pos) (y pos)))))

(defun random-pos ()
  (loop for x = (random *stage-width*)
        for y = (random *stage-height*)
        unless (member-if (op (typep _ 'solid)) (get-objects-at-pos (pos x y)))
          do (return (pos x y))))
