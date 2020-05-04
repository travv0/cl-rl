;;;; rl-curses.lisp

(in-package #:rl-curses)

(defun start-color ()
  (when (eql (charms/ll:has-colors) charms/ll:FALSE)
    (error "Your terminal does not support color."))
  (let ((ret-code (charms/ll:start-color)))
    (if (= ret-code 0)
        t
        (error "start-color error ~s." ret-code))))

(defun color-from-keyword (color)
  (ecase color
    (:black   charms/ll:COLOR_BLACK)
    (:red     charms/ll:COLOR_RED)
    (:green   charms/ll:COLOR_GREEN)
    (:yellow  charms/ll:COLOR_YELLOW)
    (:blue    charms/ll:COLOR_BLUE)
    (:magenta charms/ll:COLOR_MAGENTA)
    (:cyan    charms/ll:COLOR_CYAN)
    (:white   charms/ll:COLOR_WHITE)))

(defun define-color-pair (pair foreground background)
  (progn (charms/ll:init-pair pair (color-from-keyword foreground) (color-from-keyword background))
          (charms/ll:color-pair pair)))

(defparameter *color-pairs* (serapeum:dict))
(defparameter *color-pair-counter* 0)

(defmacro with-colors ((color-pair &key window bold) &body body)
  (let ((winptr (gensym))
        (color-pair-int (gensym)))
    (alexandria:once-only (color-pair)
      `(let ((,winptr (charms::window-pointer (or ,window (charms:standard-window))))
             (,color-pair-int (gethash ,color-pair *color-pairs*)))
         (unless ,color-pair-int
           (destructuring-bind (fg bg) ,color-pair
             (setf ,color-pair-int (define-color-pair (incf *color-pair-counter*) fg bg)
                   (gethash ,color-pair *color-pairs*) ,color-pair-int)))
         (charms/ll:wattron ,winptr ,color-pair-int)
         (when ,bold
           (charms/ll:wattron ,winptr charms/ll:a_bold))
         ,@body
         (charms/ll:wattroff ,winptr ,color-pair-int)
         (when ,bold
           (charms/ll:wattroff ,winptr charms/ll:a_bold))))))

(defun display-each (objects)
  (dolist (obj objects)
    (display obj)))

(defun display (obj &optional memory-p)
  (with-accessors ((x rl:x) (y rl:y)) obj
    (destructuring-bind (char fg bg bold)
        (etypecase obj
          (rl:player (list #\@ :white :black t))
          (rl:cell (list #\. :white :black nil))
          (rl:wall (list #\# :yellow :black nil))
          (rl:door (list (if (typep obj 'rl::solid) #\+ #\') :red :black nil))
          (rl:goblin (list #\g :green :black nil))
          (rl:rat (list #\r :white :black nil))
          (rl:item (list #\! :yellow :black t))
          (rl:memory
           (display (make-instance (rl:memory-of obj) :x x :y y) t)
           (return-from display)))
      (draw x y char fg bg bold memory-p))))

(defun draw (x y char foreground-color background-color bold &optional memory-p)
  (let ((foreground-color (if memory-p :blue foreground-color))
        (background-color (if memory-p :black background-color))
        (bold (if memory-p nil bold)))
    (multiple-value-bind (width height)
        (charms:window-dimensions charms:*standard-window*)
      (let ((x (+ (- x (rl:x (rl:player))) (floor width 2)))
            (y (+ (- y (rl:y (rl:player))) (floor height 2))))
        (when (and (<= 0 x (1- width)) (<= 0 y (1- height)))
          (with-colors ((list foreground-color background-color) :bold bold)
            (charms:write-char-at-point charms:*standard-window*
                                        char
                                        x
                                        y)))))))

(defun display-health (health)
  (with-colors ('(:white :black))
    (charms:write-string-at-point charms:*standard-window*
                                  "Health: "
                                  0
                                  0))
  (with-colors ('(:green :black))
    (charms:write-string-at-point charms:*standard-window*
                                  (make-string (ceiling health 10) :initial-element #\=)
                                  8
                                  0))
  (with-colors ('(:red :black))
    (charms:write-string-at-point charms:*standard-window*
                                  (make-string (- 10 (ceiling health 10)) :initial-element #\=)
                                  (+ 8 (ceiling health 10))
                                  0)))

(defvar *key-action-map* (make-hash-table))

(defun char-to-action (char)
  (or (gethash char *key-action-map*)
      (gethash (code-char char) *key-action-map*)))

(defun map-keys (input)
  (let ((key-action-map (make-hash-table)))
    (flet ((true-key (key)
             (if (symbolp key)
                 (symbol-value (find-symbol (concatenate 'string "KEY_" (symbol-name key))
                                            :charms/ll))
                 key)))
      (loop for (action keys) in input
            do (if (listp keys)
                   (loop for key in keys
                         do (setf (gethash (true-key key) key-action-map)
                                  (make-keyword action)))
                   (setf (gethash (true-key keys) key-action-map)
                         (make-keyword action)))))
    key-action-map))

(defun load-keys (&optional (file-name "keys.lisp"))
  (with-standard-io-syntax
    (uiop:with-input-file (file file-name)
      (setf *key-action-map* (map-keys (read file))))))

(defun get-char-code ()
  (ignore-errors (char-code (charms:get-char charms:*standard-window* :ignore-error t))))

(defun clear-screen (window)
  (multiple-value-bind (width height)
      (charms:window-dimensions window)
    (loop for y below height do
      (loop for x below width do
        (draw x y #\Space :black :black nil)))))

(defun main ()
  (load-keys)
  (handler-case
      (charms:with-curses ()
        (charms:disable-echoing)
        (charms:enable-raw-input :interpret-control-characters t)
        (charms:enable-non-blocking-mode charms:*standard-window*)
        (charms/ll:halfdelay 1)
        (charms/ll:curs-set 0)
        (charms/ll:keypad charms/ll:*stdscr* 1)
        (start-color)

        (rl:initialize)

        (charms:clear-window charms:*standard-window* :force-repaint t)
        (clear-screen charms:*standard-window*)
        (let ((state (rl:tick nil)))
          (display-each (getf state :objects)))
        (charms:refresh-window charms:*standard-window*)

        (loop for c = (get-char-code)
              when c
                do (charms:clear-window charms:*standard-window* :force-repaint t)
                   (clear-screen charms:*standard-window*)
                   (let ((state (rl:tick (char-to-action c))))
                     (display-each (getf state :objects))
                     (display-health (getf state :health)))
                   (charms:refresh-window charms:*standard-window*)))
    (rl::quit-condition ())))
