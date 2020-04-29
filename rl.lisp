;;;; rl.lisp

(in-package #:rl)

(init-ecs)

(defcomponent position (x y z))
(defcomponent velocity (x y z))

(defcomponent friction (amount))

(defcomponent visible (char color))

(defcomponent player ())
(defcomponent solid ())

(defcomponent colliding (entity))

(defsys move ((position velocity) (e))
  (incf (position/x e) (round (velocity/x e)))
  (incf (position/y e) (round (velocity/y e)))
  (incf (position/z e) (round (velocity/z e))))

(defsys apply-friction ((velocity) (e))
  (decf (velocity/x e) (* (velocity/x e) (friction/amount e)))
  (decf (velocity/y e) (* (velocity/y e) (friction/amount e))))

(defsys reset-collisions ((colliding) (e))
  (remove-component e 'colliding))

(defsys collision-check ((position solid) (e1 e2))
  (when (and (= (position/x e1) (position/x e2))
             (= (position/y e1) (position/y e2)))
    (format t "~a is colliding with ~a~%" e1 e2)
    (add-component e1 'colliding (list :entity e2))
    (add-component e2 'colliding (list :entity e1))))

(defvar *display-function*
  (lambda (x y char color)
    (declare (ignore x y char color))
    (error "*display-function* must be set to a function that draws entities"))
  "function that displays entites")

(defsys display ((position visible) (e))
  (funcall *display-function*
           (position/x e)
           (position/y e)
           (visible/char e)
           (visible/color e)))

(defparameter *player*
  (progn (when *player*
           (remove-entity *player*))
         (add-entity nil
           (player)
           (solid)
           (position :x 50 :y 20 :z 0)
           (velocity :x 0 :y 0 :z 0)
           (friction :amount 1)
           (visible :char #\@ :color :white))))

(defparameter *systems*
  '(reset-collisions
    move
    collision-check
    apply-friction
    display)
  "a list of all system names in the order they should run")

(serapeum:defconst key-y (char-code #\y))
(serapeum:defconst key-u (char-code #\u))
(serapeum:defconst key-h (char-code #\h))
(serapeum:defconst key-j (char-code #\j))
(serapeum:defconst key-k (char-code #\k))
(serapeum:defconst key-l (char-code #\l))
(serapeum:defconst key-b (char-code #\b))
(serapeum:defconst key-n (char-code #\n))
(serapeum:defconst key-up 259)
(serapeum:defconst key-left 260)
(serapeum:defconst key-down 258)
(serapeum:defconst key-right 261)

(defun update (display-function key-code &optional init)
  (when init
    (init-floor))
  (case key-code
    ((nil))
    (#.(list key-h key-left) (setf (velocity/x *player*) -1))
    (#.(list key-k key-up) (setf (velocity/y *player*) -1))
    (#.(list key-l key-right) (setf (velocity/x *player*) 1))
    (#.(list key-j key-down) (setf (velocity/y *player*) 1))
    (#.key-y (setf (velocity/x *player*) -1 (velocity/y *player*) -1))
    (#.key-u (setf (velocity/x *player*) 1 (velocity/y *player*) -1))
    (#.key-b (setf (velocity/x *player*) -1 (velocity/y *player*) 1))
    (#.key-n (setf (velocity/x *player*) 1 (velocity/y *player*) 1))
    (t (format t "Unknown key: ~a (~d)~%" (code-char key-code) key-code)))

  (let ((*display-function* display-function))
    (mapc #'do-system *systems*)))

(defun init-floor ()
  (let ((stage (dungen:make-stage :density 1
                                  :wild-factor 1
                                  :room-extent 9
                                  :door-rate 0.1
                                  :width 79
                                  :height 23)))
    (loop for y from (1- (dungen:stage-height stage)) downto 0 do
      (loop for x below (dungen:stage-width stage)
            for cell = (dungen:get-cell stage x y)
            do (cond ((dungen:has-feature-p cell :wall)
                      (make-wall x y)))))))

(defun make-wall (x y)
  (add-entity nil
    (position :x x :y y :z 0)
    (solid)
    (visible :char #\# :color :white)))
