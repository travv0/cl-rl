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

(defun get-display-char (name attributes)
  (flet ((hostile (char color bold)
           (values char color (if (getf attributes :attacking) :yellow :black) bold)))
    (case name
      (:player (hostile #\@ :white t))
      (:cell (values #\. :white :black nil))
      (:wall (values #\# :yellow :black nil))
      (:door (values (if (getf attributes :open) #\' #\+) :red :black nil))
      (:goblin (hostile #\g :green nil))
      (:goblin-fighter (hostile #\g :green t))
      (:rat (hostile #\r :white nil))
      (:warrior (hostile #\w :red nil))
      (:potion (values #\! :yellow :black t))
      ((:dagger :sword) (values #\) :yellow :black nil))
      (:kite-shield (values #\] :yellow :black nil))
      (:memory
       (display (getf attributes :memory-of) t)
       (return-from get-display-char))
      (t
       #-release (error "~s fell through case expression" name)
       #+release (values #\? :white :black nil)))))

(desfun display ((&key name x y attributes) &optional memory-p)
  (multiple-value-bind (char fg bg bold)
      (get-display-char name attributes)
    (when char
      (draw x y char fg bg bold memory-p))))

(defvar *player-x*)
(defvar *player-y*)

(defun draw (x y char foreground-color background-color bold &optional memory-p)
  (let ((foreground-color (if memory-p :blue foreground-color))
        (background-color (if memory-p :black background-color))
        (bold (if memory-p nil bold)))
    (multiple-value-bind (width height)
        (charms:window-dimensions charms:*standard-window*)
      (let ((x (+ (- x *player-x*) (floor width 2)))
            (y (+ (- y *player-y*) (floor height 2))))
        (when (and (<= 0 x (1- width)) (<= 0 y (1- height)))
          (with-colors ((list foreground-color background-color) :bold bold)
            (charms:write-char-at-point charms:*standard-window*
                                        char
                                        x
                                        y)))))))

(defun display-bar (x y label color current max
                    &key with-numbers (previous current) (diff-color :yellow))
  (let ((current (max (min current max) 0))
        (diff (max 0 (- previous current))))
    (with-colors ('(:white :black))
      (charms:write-string-at-point charms:*standard-window*
                                    label
                                    x
                                    y))
    (with-colors ((list color :black))
      (charms:write-string-at-point charms:*standard-window*
                                    (make-string (ceiling current 10)
                                                 :initial-element #\=)
                                    (+ x (length label))
                                    y))
    (with-colors ((list diff-color :black))
      (charms:write-string-at-point charms:*standard-window*
                                    (make-string (floor diff 10)
                                                 :initial-element #\=)
                                    (+ x (length label) (ceiling current 10))
                                    y))
    (with-colors ('(:black :black) :bold t)
      (charms:write-string-at-point charms:*standard-window*
                                    (make-string (- (ceiling max 10)
                                                    (ceiling current 10)
                                                    (floor diff 10))
                                                 :initial-element #\=)
                                    (+ x (length label) (ceiling current 10) (floor diff 10))
                                    y)))
  (when with-numbers
    (with-colors ('(:white :black))
      (charms:write-string-at-point charms:*standard-window*
                                    (format nil "~d/~d" current max)
                                    (+ 1 x (length label) (ceiling max 10))
                                    y))))

(defun display-health (health max-health previous-health)
  (display-bar 0 0 "H:" :red health max-health :with-numbers t
                                               :previous previous-health))

(defun display-stamina (stamina max-stamina previous-stamina)
  (display-bar 0 1 "S:" :green stamina max-stamina :with-numbers t
                                                   :previous previous-stamina))

(defun display-log (rows log)
  (multiple-value-bind (width height)
      (charms:window-dimensions charms:*standard-window*)
    (loop with y = (1- height)
          for (l next-l) on log
          while (<= (- height y) rows) do
            (charms:write-string-at-point charms:*standard-window* l 0 y)
            (charms:clear-line-after-cursor charms:*standard-window*)
            (decf y (ceiling (length next-l) width)))))

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

(defun update-and-display (char)
  (multiple-value-bind (width height)
      (charms:window-dimensions charms:*standard-window*)
    (declare (ignorable width height))
    (destructuring-bind (&key ((:player (&whole player
                                         &key ((:attributes player-attributes))
                                         &allow-other-keys)))
                           objects log turn)
        (rl:tick (char-to-action char))
      (let ((*player-x* (getf player :x))
            (*player-y* (getf player :y)))
        (charms:clear-window charms:*standard-window* :force-repaint t)
        (clear-screen charms:*standard-window*)
        (display-each objects)
        (display-health (getf player-attributes :health)
                        (getf player-attributes :max-health)
                        (getf player-attributes :previous-health))
        (display-stamina (getf player-attributes :stamina)
                         (getf player-attributes :max-stamina)
                         (getf player-attributes :previous-stamina))
        (display-log 5 log)
        (let ((turn-string (format nil "turn: ~d" turn)))
          (charms:write-string-at-point charms:*standard-window*
                                        turn-string
                                        (- width (length turn-string))
                                        0)
          (charms:refresh-window charms:*standard-window*))))))

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
        (update-and-display nil)

        (loop for c = (get-char-code)
              when c
                do (update-and-display c)))
    (rl::quit-condition ())))
