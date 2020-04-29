;;;; rl.lisp

(in-package #:rl)

(defclass game-object () ())

(defclass pos ()
  ((x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y)))

(defclass vel ()
  ((dx :initarg :dx :initform 0 :accessor dx)
   (dy :initarg :dy :initform 0 :accessor dy)))

(defclass visible ()
  ((char :initarg :char :accessor display-char)
   (color :initarg :color :initform :white :accessor display-color)))

(defclass player (game-object pos vel visible) ())

(defgeneric update (game-object)
  (:documentation "What the object should do each tick."))

(defmethod update ((obj game-object))
  (when (typep obj '(and pos vel))
    (incf (x obj) (round (dx obj)))
    (incf (y obj) (round (dy obj)))))

(defclass wall (game-object pos visible) ())

(defvar *display-function*
  (lambda (x y char color)
    (declare (ignore x y char color))
    (error "*display-function* must be set to a function that draws entities"))
  "function that displays entites")

(defmethod display ((obj game-object))
  (funcall *display-function*
           (x obj)
           (y obj)
           (display-char obj)
           (display-color obj)))

(defparameter *player*
  (make-instance 'player :x 50 :y 20 :color :white :char #\@))

(defparameter *game-objects* '())

(define-condition quit-condition () ())

(defun tick (display-function key-code &optional init)
  (when init
    (init-floor)
    (setf *player*
          (make-instance 'player :x 50 :y 20 :color :white :char #\@))
    (setf *game-objects* '())
    (push *player* *game-objects*))
  (case key-code
    ((nil))
    (:move-left (setf (dx *player*) -1))
    (:move-up (setf (dy *player*) -1))
    (:move-right (setf (dx *player*) 1))
    (:move-down (setf (dy *player*) 1))
    (:move-up-left (setf (dx *player*) -1 (dy *player*) -1))
    (:move-up-right (setf (dx *player*) 1 (dy *player*) -1))
    (:move-down-left (setf (dx *player*) -1 (dy *player*) 1))
    (:move-down-right (setf (dx *player*) 1 (dy *player*) 1))
    (:quit (error 'quit-condition))
    (t (format t "Unknown key: ~a (~d)~%" (code-char key-code) key-code)))

  (dolist (obj *game-objects*)
    (update obj)
    (let ((*display-function* display-function))
      (display obj))))

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
  (make-instance 'wall :x x :y y :char #\#))
