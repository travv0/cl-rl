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
  "Converts the keyword `color' to the integer representation for the
terminal."
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
  (progn (charms/ll:init-pair pair
                              (color-from-keyword foreground)
                              (color-from-keyword background))
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
  (ecase-of rl:visible-keyword name
    (:player (values #\@ :white :black t))
    (:grass (values #\. :green :black nil))
    (:wall (values #\# :yellow :black nil))
    (:water (values #\~ :blue :black nil))
    (:shallow-water (values #\. :blue :black t))
    (:tree (values #\" :green :black nil))
    (:sand (values #\. :yellow :black nil))
    (:door (values (if (getf attributes :open) #\' #\+) :red :black nil))
    (:goblin (values #\g :green :black nil))
    (:goblin-fighter (values #\g :green :black t))
    (:goblin-brawler (values #\g :yellow :black nil))
    (:rat (values #\r :white :black nil))
    (:warrior (values #\w :red :black nil))
    (:health-potion (values #\! :yellow :black nil))
    ((:dagger :sword) (values #\) :yellow :black nil))
    (:kite-shield (values #\] :yellow :black nil))
    ((:shield :weapon :enemy :item)
     (error "~a is a base class and shouldn't be directly instantiated" name))))

(desfun display ((&key name x y attributes) &optional memory-p)
  (multiple-value-bind (char fg bg bold)
      (get-display-char name attributes)
    (when char
      (draw x y char fg bg bold memory-p))))

(defvar *player-x*)
(defvar *player-y*)

;; Safe string writing that clips to window bounds
(defun safe-write-string-at-point (window string x y)
  "Write string at point, clipping to window bounds"
  (handler-case
      (multiple-value-bind (width height)
          (charms:window-dimensions window)
        (when (and (< y height) (>= y 0) (< x width) (>= x 0))
          (let* ((safe-string (or string ""))  ;; Handle nil strings
                 (max-len (max 0 (- width x)))
                 (clipped-string (if (> (length safe-string) max-len)
                                    (subseq safe-string 0 max-len)
                                    safe-string)))
            (when (> (length clipped-string) 0)
              (charms:write-string-at-point window clipped-string x y)))))
    (error (e)
      ;; Silently ignore write errors
      nil)))

;; Safe character writing that clips to window bounds
(defun safe-write-char-at-point (window char x y)
  "Safely write a character at point"
  (handler-case
      (multiple-value-bind (width height)
          (charms:window-dimensions window)
        (when (and (< y height) (>= y 0) (< x width) (>= x 0))
          (charms:write-char-at-point window char x y)))
    (error ()
      nil)))

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
            (safe-write-char-at-point charms:*standard-window*
                                      char
                                      x
                                      y)))))))

(defun display-bar (x y label color current max
                    &key with-numbers (previous current) (diff-color :yellow))
  (multiple-value-bind (width height)
      (charms:window-dimensions charms:*standard-window*)
    (when (and (< y height) (>= y 0) (< x width))
      (let ((current (max (min current max) 0))
            (diff (max 0 (- previous current))))
        ;; Write label
        (with-colors ('(:white :black))
          (safe-write-string-at-point charms:*standard-window*
                                      label
                                      x
                                      y))
        
        ;; Calculate available space and scaling factor
        (let* ((label-len (length label))
               (numbers-text (format nil "~d/~d" current max))
               (numbers-space (if with-numbers 
                                 (+ 2 (length numbers-text))
                                 0))
               ;; Reserve space for: label, bar, space, numbers
               (available-width (- width x label-len numbers-space 1))
               ;; Use 1/3 of available width for the bar, but at least 10 chars
               (bar-width (max 10 (min available-width 
                                      (floor available-width 3))))
               ;; Calculate how much health/stamina each character represents
               (scale-factor (if (zerop max) 1 (/ (float max) bar-width)))
               ;; Calculate character positions for current and diff
               (current-chars (if (zerop max) 
                                 0
                                 (round (/ current scale-factor))))
               (diff-chars (if (zerop max)
                              0 
                              (round (/ diff scale-factor))))
               ;; Ensure we don't exceed bar width
               (current-chars (min current-chars bar-width))
               (diff-chars (min diff-chars (- bar-width current-chars))))
          
          ;; Draw current health/stamina
          (when (> current-chars 0)
            (with-colors ((list color :black))
              (safe-write-string-at-point charms:*standard-window*
                                          (make-string current-chars
                                                      :initial-element #\=)
                                          (+ x label-len)
                                          y)))
          
          ;; Draw damage taken
          (when (> diff-chars 0)
            (with-colors ((list diff-color :black))
              (safe-write-string-at-point charms:*standard-window*
                                          (make-string diff-chars
                                                      :initial-element #\=)
                                          (+ x label-len current-chars)
                                          y)))
          
          ;; Draw empty portion
          (let ((empty-chars (- bar-width current-chars diff-chars)))
            (when (> empty-chars 0)
              (with-colors ('(:black :black) :bold t)
                (safe-write-string-at-point charms:*standard-window*
                                            (make-string empty-chars
                                                        :initial-element #\=)
                                            (+ x label-len current-chars diff-chars)
                                            y))))
          
          ;; Draw numbers if requested
          (when with-numbers
            (with-colors ('(:white :black))
              (safe-write-string-at-point charms:*standard-window*
                                          numbers-text
                                          (+ x label-len bar-width 1)
                                          y))))))))

(defun display-health (health max-health previous-health)
  (display-bar 0 0 "H:" :red health max-health :with-numbers t
                                               :previous previous-health))

(defun display-stamina (stamina max-stamina previous-stamina)
  (display-bar 0 1 "S:" :green stamina max-stamina :with-numbers t
                                                   :previous previous-stamina))

(defun display-log (rows log)
  (multiple-value-bind (width height)
      (charms:window-dimensions charms:*standard-window*)
    (when (and (> height 0) (> width 0))  ;; Ensure valid dimensions
      (loop with y = (1- height)
            for l in log
            for line-str = (or l "")  ;; Handle nil entries
            for lines-used = (max 1 (ceiling (max 1 (length line-str)) width))
            while (and (<= (- height y) rows) (>= y 0)) do
              (when (and (>= y 0) (< y height))  ;; Extra safety check
                (safe-write-string-at-point charms:*standard-window* line-str 0 y)
                (handler-case
                    (charms:clear-line-after-cursor charms:*standard-window*)
                  (error () nil)))  ;; Ignore clear errors
              (decf y lines-used)))))

(defvar *key-action-map* (make-hash-table))

(defvar *state* :play)

(defun char-to-action (char)
  (and char
       (or (@ *key-action-map* *state* char)
           (@ *key-action-map* *state* (code-char char))
           (and (eql *state* :inventory) (code-char char)))))

(defun map-keys (input)
  (let ((key-action-map (make-hash-table)))
    (flet ((true-key (key)
             (if (symbolp key)
                 (symbol-value (find-symbol (concatenate 'string "KEY_" (symbol-name key))
                                            :charms/ll))
                 key)))
      (loop for (state input) on input by #'cddr do
        (loop for (action keys) in input
              do (unless (gethash state key-action-map)
                   (setf (gethash state key-action-map) (make-hash-table)))
                 (if (listp keys)
                     (loop for key in keys
                           do (setf (@ key-action-map state (true-key key))
                                    (make-keyword action)))
                     (setf (@ key-action-map state (true-key keys))
                           (make-keyword action))))))
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

(defun draw-play (data width height)
  (declare (ignorable width height))
  (handler-case
      (destructuring-bind (&key ((:player (&whole player
                                           &key ((:attributes player-attributes))
                                           &allow-other-keys)))
                             objects log turn seed)
          data
        (when player  ;; Only draw if player exists
          (let ((*player-x* (getf player :x))
                (*player-y* (getf player :y)))
            (charms:clear-window charms:*standard-window* :force-repaint t)
            (clear-screen charms:*standard-window*)
            
            ;; Display all objects
            (display-each objects)
            
            ;; Display the player separately since it's not in objects list
            (display player)
            
            ;; Display UI elements
            (display-health (or (getf player-attributes :health) 100)
                            (or (getf player-attributes :max-health) 100)
                            (or (getf player-attributes :previous-health) 100))
            (display-stamina (or (getf player-attributes :stamina) 100)
                             (or (getf player-attributes :max-stamina) 100)
                             (or (getf player-attributes :previous-stamina) 100))
            (display-log 5 (or log '()))

            (let ((chunk (rl::player-chunk)))
              (let ((turn-string (format nil "pos: ~d, ~d (~d, ~d) | turn: ~d | seed: ~4d"
                                         *player-x* *player-y* (rl::x chunk) (rl::y chunk) 
                                         (or turn 0) (or seed 0))))
                (safe-write-string-at-point charms:*standard-window*
                                            turn-string
                                            (max 0 (- width (length turn-string)))
                                            0))))))
    (error (e)
      ;; If there's an error, just clear the screen
      (format *error-output* "Draw-play error: ~a~%" e)
      (charms:clear-window charms:*standard-window* :force-repaint t))))

(defun draw-inventory (data width height)
  (declare (ignorable width height))
  (charms:clear-window charms:*standard-window* :force-repaint t)
  (loop for item in (getf data :inventory)
        for i from 0
        do (destructuring-bind (char
                                &key
                                  ((:attributes (&key charges max-charges equipped)))
                                  ((:display-name name))
                                &allow-other-keys)
               item
             (charms:write-string-at-point charms:*standard-window*
                                           (format nil "~c. ~a ~@[~a~]~@[/~a~]~@[(equipped: ~a)~]"
                                                   char
                                                   name
                                                   charges
                                                   max-charges
                                                   equipped)
                                           0
                                           i))))

(defun update-and-display (char)
  (handler-case
      (multiple-value-bind (width height)
          (charms:window-dimensions charms:*standard-window*)
        (destructuring-bind (state data) (rl:tick (char-to-action char))
          (setf *state* state)
          (ecase state
            (:play (draw-play data width height))
            (:inventory (draw-inventory data width height)))))
    (error (e)
      ;; Log error but don't crash
      (format *error-output* "Display error: ~a~%" e)))
  
  (charms:refresh-window charms:*standard-window*))

(defun dev ()
  (bt:make-thread (lambda () (main))
                  :name "game thread"))

(defun main (&optional seed)
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

        ;; Initialize game state
        (rl:initialize seed)
        
        ;; Give time for initialization to complete
        (sleep 0.1)
        
        ;; Initial display
        (handler-case
            (update-and-display nil)
          (error (e)
            (format *error-output* "Initial display error: ~a~%" e)
            ;; Try a simple clear instead
            (charms:clear-window charms:*standard-window* :force-repaint t)
            (charms:refresh-window charms:*standard-window*)))

        ;; Main game loop
        (loop for c = (get-char-code)
              when c
                do (update-and-display c)))
    (rl::quit-condition ())))
