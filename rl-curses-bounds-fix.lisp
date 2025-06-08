;; Patch for rl-curses to fix bounds checking and dynamic scaling
;; Load this after loading rl-curses

(in-package :rl-curses)

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

;; Redefine display-bar to use dynamic scaling based on window width
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

;; Also fix display-log to handle window bounds  
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

;; Fix the turn info string at top right to not overflow
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
            (display-each objects)
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
      (charms:clear-window charms:*standard-window* :force-repaint t))))

;; Override write-char-at-point to be safer
(defun safe-write-char-at-point (window char x y)
  "Safely write a character at point"
  (handler-case
      (multiple-value-bind (width height)
          (charms:window-dimensions window)
        (when (and (< y height) (>= y 0) (< x width) (>= x 0))
          (charms:write-char-at-point window char x y)))
    (error ()
      nil)))

;; Override draw to use safe writing
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

;; Override update-and-display to be more robust
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

;; Override main to ensure proper initialization
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