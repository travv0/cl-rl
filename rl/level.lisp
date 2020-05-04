(in-package #:rl)

(defvar *pos-cache*)
(defvar *game-objects*)

(defclass memory (visible)
  ((%foreground-color :initform :blue)
   (%background-color :initform :black)
   (%bold-color :initform nil)))

(defclass wall (visible solid opaque)
  ((%char :initform #\#)
   (%foreground-color :initform :yellow)
   (%background-color :initform :black)
   (%bold-color :initform nil)))

(defclass door (visible)
  ((%char :initform #\+)
   (%foreground-color :initform :red)
   (%background-color :initform :black)
   (%bold-color :initform nil)))

(defclass cell (visible)
  ((%char :initform #\.)))

(defun replace-memory (obj)
  (let ((cache (aref *pos-cache* (x obj) (y obj))))
    (setf (aref *pos-cache* (x obj) (y obj))
          (remove-if (op (typep _ 'memory)) cache)
          (aref *pos-cache* (x obj) (y obj))
          (append (aref *pos-cache* (x obj) (y obj))
                  (list (make-instance 'memory :char (display-char obj) :x (x obj) :y (y obj)))))))

(defun should-display (obj)
  (or (typep obj 'can-see) (typep obj 'memory)))

(defparameter *stage-width* 79)
(defparameter *stage-height* 79)

(defun init-cells (width height)
  (loop for y below height do
    (loop for x below width do
      (let ((cell (make-instance 'cell :x x :y y)))
        (add-object cell)))))

(defun init-floor (width height)
  (let ((stage (dungen:make-stage :density 0.8
                                  :wild-factor 0
                                  :room-extent 19
                                  :door-rate 1.0
                                  :width width
                                  :height height)))
    (loop for y from (1- (dungen:stage-height stage)) downto 0 do
      (loop for x below (dungen:stage-width stage)
            for cell = (dungen:get-cell stage x y)
            do (cond ((dungen:has-feature-p cell :wall)
                      (add-object (make-wall x y)))
                     ((or (dungen:has-feature-p cell :door/vertical)
                          (dungen:has-feature-p cell :door/horizontal))
                      (add-object (make-door x y))))))))

(defun make-wall (x y)
  (make-instance 'wall :x x :y y))

(defun make-door (x y)
  (make-instance (mix 'door 'opaque 'solid) :x x :y y))

(defun get-objects-at-pos (pos)
  (when (and (<= 0 (x pos) (1- *stage-width*))
             (<= 0 (y pos) (1- *stage-height*)))
    (aref *pos-cache* (x pos) (y pos))))

(defun get-object-at-pos (pos)
  (when (and (<= 0 (x pos) (1- *stage-width*))
             (<= 0 (y pos) (1- *stage-height*)))
    (find-if #'should-display (aref *pos-cache* (x pos) (y pos)))))

(defun random-pos ()
  (loop for x = (random *stage-width*)
        for y = (random *stage-height*)
        unless (member-if (op (typep _ 'solid)) (get-objects-at-pos (pos x y)))
          do (return (pos x y))))