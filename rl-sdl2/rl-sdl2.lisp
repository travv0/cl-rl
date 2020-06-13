;;;; rl-sdl2.lisp

(in-package #:rl-sdl2)

(defparameter *tile-size* 32)

(defmacro init-textures ()
  (let (result)
    (cl-fad:walk-directory
     "rl-sdl2/assets/"
     (lambda (file)
       (let* ((file-name (file-namestring file))
              (image-name (subseq file-name 0 (position #\. file-name)))
              (surface (gensym "surface")))
         (push `(let ((,surface (sdl2-image:load-image ,file)))
                  (defparameter ,(intern (concatenate 'string "*" (string-upcase image-name) "-IMAGE*"))
                    (sdl2:create-texture-from-surface *renderer* ,surface))
                  (sdl2:free-surface ,surface))
               result)))
     :test (lambda (file)
             (let ((file-name (file-namestring file)))
               (string-equal (subseq file-name (position #\. file-name))
                             ".png"))))
    `(progn ,@result)))

(defun display-each (objects)
  (dolist (obj objects)
    (display obj)))

(defun get-image (name attributes)
  (ecase-of rl:visible-keyword name
    (:player *player-image*)
    (:cell *floor-image*)
    (:wall *tile-image*)
    (:door (unless (getf attributes :open) *bug-image*))
    ((:goblin :goblin-fighter :rat :warrior) *worm-image*)
    ((:dagger :sword :health-potion :kite-shield) *bug-image*)
    ((:item :weapon :shield :enemy)
     (error "~a should be inherited from and cannot be drawn" name))))

(desfun display ((&key name x y attributes))
  (when-let ((image (get-image name attributes)))
    (draw x y image)))

(defvar *player-x*)
(defvar *player-y*)

(defvar *window*)
(defvar *renderer*)
(defvar *font*)

(defun draw (x y image)
  (multiple-value-bind (width height)
      (sdl2:get-window-size *window*)
    (let ((x (+ (- (* x *tile-size*) (* *player-x* *tile-size*))
                (floor width 2)))
          (y (+ (- (* y *tile-size*) (* *player-y* *tile-size*))
                (floor height 2))))
      (when (and (<= (- *tile-size*) x (1- width)) (<= (- *tile-size*) y (1- height)))
        (sdl2:render-copy *renderer* image
                          :dest-rect (sdl2:make-rect x y *tile-size* *tile-size*))))))

(defvar *key-action-map* (make-hash-table))

(defvar *state* :play)

(defun scancode-to-action (scancode)
  (and scancode
       (or (@ *key-action-map* *state* scancode)
           (and (eql *state* :inventory) (scancode-char scancode)))))

(defparameter *letters*
  (loop for c from (char-code #\a) to (char-code #\z)
        collecting (code-char c)))

(defun scancode-char (scancode)
  (let* ((upcase-p (and (typep scancode 'cons)
                        (eq (first scancode) :shift)))
         (scancode (ctypecase scancode
                     (cons (second scancode))
                     (t scancode)))
         (character (nth (- scancode 4) *letters*)))
    (if (and upcase-p character)
        (char-upcase character)
        character)))

(defun translate-key (key)
  (case key
    (#\. 'period)
    ((#\Esc esc) 'escape)
    (otherwise key)))

(defun true-key (key)
  (let* ((true-key (symbol-value (find-symbol (concatenate 'string
                                                           "+SDL-SCANCODE-"
                                                           (let ((key (translate-key key)))
                                                             (etypecase key
                                                               (symbol (symbol-name key))
                                                               (character (string-upcase key))
                                                               (cons (symbol-name (second key)))))
                                                           "+")
                                              :sdl2-ffi)))
         (true-key (cond
                     ((typep key 'cons) (list (make-keyword (first key)) true-key))
                     ((and (typep key 'character) (upper-case-p key))
                      (list :shift true-key))
                     (t true-key))))
    (or true-key (error "unknown key: ~s" key))))

(defun map-keys (input)
  (let ((key-action-map (make-hash-table)))
    (loop for (state input) on input by #'cddr do
      (loop for (action keys) in input
            do (unless (gethash state key-action-map)
                 (setf (gethash state key-action-map) (make-hash-table :test 'equal)))
               (if (listp keys)
                   (loop for key in keys
                         do (setf (@ key-action-map state (true-key key))
                                  (make-keyword action)))
                   (setf (@ key-action-map state (true-key keys))
                         (make-keyword action)))))
    key-action-map))

(defun load-keys (&optional (file-name "keys.lisp"))
  (with-standard-io-syntax
    (uiop:with-input-file (file file-name)
      (setf *key-action-map* (map-keys (read file))))))

(defun display-health (health max-health previous-health)
  (display-bar 5 5 max-health 20 health max-health previous-health '(255 0 0) '(192 192 192)))

(defun display-stamina (stamina max-stamina previous-stamina)
  (display-bar 5 30 max-stamina 20 stamina max-stamina previous-stamina '(0 255 0) '(192 192 192)))

(defun display-bar (x y w h amount max-amount previous-amount fg-rgb bg-rgb
                    &optional (diff-rgb '(255 255 0)))
  (destructuring-bind ((fg-r fg-g fg-b) (bg-r bg-g bg-b) (diff-r diff-g diff-b))
      (list fg-rgb bg-rgb diff-rgb)
    (multiple-value-bind (old-r old-g old-b old-a)
        (sdl2:get-render-draw-color *renderer*)
      (let ((bg-rect (sdl2:make-rect x y w h))
            (diff-rect (sdl2:make-rect x y (round (* w (/ previous-amount max-amount))) h))
            (current-rect (sdl2:make-rect x y (round (* w (/ amount max-amount))) h)))
        (sdl2:set-render-draw-color *renderer* bg-r bg-g bg-b 1)
        (sdl2:render-fill-rect *renderer* bg-rect)
        (sdl2:set-render-draw-color *renderer* diff-r diff-g diff-b 1)
        (sdl2:render-fill-rect *renderer* diff-rect)
        (sdl2:set-render-draw-color *renderer* fg-r fg-g fg-b 1)
        (sdl2:render-fill-rect *renderer* current-rect)
        (sdl2:set-render-draw-color *renderer* old-r old-g old-b old-a)))))

(defun display-turn-number (turn width height)
  (declare (ignorable width height))
  (let* ((surface (sdl2-ttf:render-text-blended *font* (format nil "turn: ~d" turn)
                                                255 255 255 255))
         (texture (sdl2:create-texture-from-surface *renderer* surface)))
    (sdl2:render-copy *renderer* texture
                      :source-rect (cffi:null-pointer)
                      :dest-rect (sdl2:make-rect (- width (sdl2:texture-width texture) 5)
                                                 5
                                                 (sdl2:texture-width texture)
                                                 (sdl2:texture-height texture)))
    (sdl2:destroy-texture texture)))

(defun display-log (count log width height)
  (declare (ignorable width height))
  (loop for entry in log
        for i from 1 to count
        do (let* ((surface (sdl2-ttf:render-text-blended *font* entry 255 255 255 255))
                  (texture (sdl2:create-texture-from-surface *renderer* surface)))
             (sdl2:render-copy *renderer* texture
                               :source-rect (cffi:null-pointer)
                               :dest-rect (sdl2:make-rect 5
                                                          (- height (* (+ (sdl2:texture-height texture) 5) i))
                                                          (sdl2:texture-width texture)
                                                          (sdl2:texture-height texture)))
             (sdl2:destroy-texture texture))))

(defun draw-play (data width height)
  (declare (ignorable width height))
  (destructuring-bind (&key ((:player (&whole player
                                       &key ((:attributes player-attributes))
                                       &allow-other-keys)))
                         objects log turn)
      data
    (let ((*player-x* (getf player :x))
          (*player-y* (getf player :y)))
      (display-each objects)
      (display-health (getf player-attributes :health)
                      (getf player-attributes :max-health)
                      (getf player-attributes :previous-health))
      (display-stamina (getf player-attributes :stamina)
                       (getf player-attributes :max-stamina)
                       (getf player-attributes :previous-stamina))
      (display-log 5 log width height)
      (display-turn-number turn width height))))

(defun draw-inventory (data width height)
  (declare (ignorable width height))
  (loop for item in (getf data :inventory)
        for i from 0
        do (destructuring-bind (char
                                &key
                                  ((:attributes (&key charges max-charges equipped)))
                                  ((:display-name name))
                                &allow-other-keys)
               item
             (let* ((surface (sdl2-ttf:render-text-blended *font* (format nil "~c. ~a ~@[~a~]~@[/~a~]~@[(equipped: ~a)~]"
                                                                          char
                                                                          name
                                                                          charges
                                                                          max-charges
                                                                          equipped)
                                                           255 255 255 255))
                    (texture (sdl2:create-texture-from-surface *renderer* surface)))
               (sdl2:render-copy *renderer* texture
                                 :source-rect (cffi:null-pointer)
                                 :dest-rect (sdl2:make-rect 5
                                                            (* (+ (sdl2:texture-height texture) 5) i)
                                                            (sdl2:texture-width texture)
                                                            (sdl2:texture-height texture)))))))

(defun update-and-display (scancode)
  (multiple-value-bind (width height)
      (sdl2:get-window-size *window*)
    (destructuring-bind (state data) (rl:tick (scancode-to-action scancode))
      (setf *state* state)
      (sdl2:render-clear *renderer*)
      (ecase-of rl:states state
        (:play (draw-play data width height))
        (:inventory (draw-inventory data width height)))
      (sdl2:render-present *renderer*))))

(defun dev ()
  (bt:make-thread (lambda () (main))
                  :name "game thread"))

(defmacro with-font ((var font-path point-size) &body body)
  `(let ((,var (sdl2-ttf:open-font ,font-path ,point-size)))
     (unwind-protect (progn ,@body)
       (sdl2-ttf:close-font ,var))))

(defun main ()
  (load-keys)
  (sdl2:with-init (:everything)
    (sdl2-image:init '(:png))
    (sdl2-ttf:init)
    (unwind-protect
         (sdl2:with-window (*window* :flags '(:shown))
           (sdl2:with-renderer (*renderer* *window* :flags '(:accelerated))
             (handler-case
                 (with-font (*font* "rl-sdl2/EBGaramond-Medium.ttf" 18)
                   (init-textures)
                   (rl:initialize)
                   (update-and-display nil)

                   (let ((shift-held nil))
                     (sdl2:with-event-loop (:method :poll)
                       (:keydown (:keysym keysym)
                                 (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-lshift)
                                   (setf shift-held t))
                                 (let ((scancode (if shift-held
                                                     (list :shift (sdl2:scancode-value keysym))
                                                     (sdl2:scancode-value keysym))))
                                   (update-and-display scancode)))

                       (:keyup (:keysym keysym)
                               (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-lshift)
                                 (setf shift-held nil)))

                       (:quit () t)

                       (:idle ()
                              (sdl2:gl-swap-window *window*)))))
               (rl::quit-condition ()))))
      (sdl2-ttf:quit)
      (sdl2-image:quit))))
