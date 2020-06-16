(in-package #:rl)

(defvar *pos-cache*)
(defvar *game-objects*)

(defclass wall (visible solid opaque)
  ())

(defclass tall-grass (visible solid opaque)
  ())

(defclass water (visible solid)
  ())

(defclass shallow-water (visible)
  ())

(defclass sand (visible)
  ())

(defclass door (visible)
  ())

(defclass cell (visible)
  ())

(defun should-display (obj)
  (typep obj 'can-see))

(defparameter *stage-width* 100)
(defparameter *stage-height* 100)

(defun init-cells (width height)
  (loop for y below height do
    (loop for x below width do
      (let ((cell (make-instance 'cell :x x :y y)))
        (add-object cell)))))

(defun grass-area-noise (x y)
  (let ((noise (* (black-tie:perlin-noise-sf (/ x 250.0) (/ y 250.0)) 0.5)))
    (incf noise (* (black-tie:perlin-noise-sf (/ x 100.0) (/ y 100.0)) 0.25))
    (incf noise (* (black-tie:perlin-noise-sf (/ x 50.0) (/ y 50.0)) 0.125))
    (incf noise (* (black-tie:perlin-noise-sf (/ x 10.0) (/ y 10.0)) 0.03))))

(defun tree-noise (x y)
  (* (black-tie:perlin-noise-sf (/ x 0.9) (/ y 0.9)) 100))

(defun lava-area-noise (x y)
  (let ((noise (* (black-tie:perlin-noise-sf (/ x 5.0) (/ y 10.0)) 1)))
    (incf noise (* (black-tie:perlin-noise-sf (/ x 2.0) (/ y 2.0)) 0.5))))

(defun init-grass-area (x y width height seed)
  (let* ((seeded-x (+ x (* width seed)))
         (seeded-y (+ y (* height seed)))
         (noise (grass-area-noise seeded-x seeded-y)))
    (cond ((< noise -0.12) (add-object (make-water x y)))
          ((< noise -0.1) (add-object (make-water x y :shallow t)))
          ((< noise -0.08) (add-object (make-sand x y)))
          ((> noise 0.15)
           (let ((tree-noise (tree-noise seeded-x seeded-y)))
             (when (> tree-noise 30)
               (add-object (make-tall-grass x y))))))))

(defun init-lava-area (x y width height seed)
  (let* ((seeded-x (+ x (* width seed)))
         (seeded-y (+ y (* height seed)))
         (noise (lava-area-noise seeded-x seeded-y)))
    (cond ((< noise -0.2) (add-object (make-water x y)))
          (t (let ((rock-noise (tree-noise seeded-x seeded-y)))
               (when (> rock-noise 40)
                 (add-object (make-tall-grass x y))))))))

(defun init-floor (width height &optional (seed 0))
  (loop for y from (1- height) downto 0 do
    (loop for x below width
          do (init-lava-area x y width height seed))))

(defun make-wall (x y)
  (make-instance 'wall :x x :y y))

(defun make-tall-grass (x y)
  (make-instance 'tall-grass :x x :y y))

(defun make-water (x y &key shallow)
  (if shallow
      (make-instance 'shallow-water :x x :y y)
      (make-instance 'water :x x :y y)))

(defun make-sand (x y)
  (make-instance 'sand :x x :y y))

(defun make-door (x y)
  (make-instance (mix 'opaque 'solid 'door) :x x :y y))

(defun get-objects-at-pos (pos)
  (when (and (<= 0 (x pos) (1- *stage-width*))
             (<= 0 (y pos) (1- *stage-height*)))
    (aref *pos-cache* (x pos) (y pos))))

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
