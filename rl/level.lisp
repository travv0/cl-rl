(in-package #:rl)

(defvar *pos-cache*)
(defvar *game-objects*)

(defclass wall (visible solid opaque)
  ())

(defclass grass (visible)
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
  (let* ((seed (write-to-string seed))
         (numerator (subseq seed 0 (- (length seed) 3)))
         (denominator (subseq seed (- (length seed) 3))))
    (/ (parse-integer numerator) (parse-integer denominator))))

(defun init-grass-area (width height seed)
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
                       (add-object (make-tall-grass x y))))))))))
    (unless (and (> (count-if (op (typep _ 'water)) *game-objects*) 700)
                 (> (count-if (op (typep _ 'tall-grass)) *game-objects*) 300))
      (clear-objects)
      (init-grass-area width height (1+ seed)))))

(defun init-lava-area (x y width height seed)
  (let* ((noise (lava-area-noise x y seed)))
    (cond ((< noise -0.2) (add-object (make-water x y)))
          (t (let ((rock-noise (tree-noise x y seed)))
               (when (> rock-noise 40)
                 (add-object (make-tall-grass x y))))))))

(defun init-floor (width height &optional (seed 0))
  (init-grass-area width height seed))

(defun make-wall (x y)
  (make-instance 'wall :x x :y y))

(defun make-grass (x y)
  (make-instance 'grass :x x :y y))

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
