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
  (sdl2:render-clear *renderer*)
  (dolist (obj objects)
    (display obj))
  (sdl2:render-present *renderer*))

(defun get-image (name attributes)
  (case name
    (:player *player-image*)
    (:cell *floor-image*)
    (:wall *tile-image*)
    (:door (unless (getf attributes :open) *bug-image*))
    ((:goblin :goblin-fighter :rat :warrior) *worm-image*)
    ((:dagger :sword :potion :kite-shield) *bug-image*)
    (:memory
     (display (getf attributes :memory-of) t)
     (return-from get-image))
    ((nil) (return-from get-image))
    (t
     #-release (error "~s fell through case expression" name)
     #+release (values #\? :white :black nil))))

(desfun display ((&key name x y attributes) &optional memory-p)
  (when-let ((image (get-image name attributes)))
    (draw x y image memory-p)))

(defvar *player-x*)
(defvar *player-y*)

(defvar *window*)
(defvar *renderer*)

(defun draw (x y image &optional memory-p)
  (multiple-value-bind (width height)
      (sdl2:get-window-size *window*)
    (let ((x (+ (- (* x *tile-size*) (* *player-x* *tile-size*))
                (floor width 2)))
          (y (+ (- (* y *tile-size*) (* *player-y* *tile-size*))
                (floor height 2))))
      (when (and (<= (- *tile-size*) x (1- width)) (<= (- *tile-size*) y (1- height)))
        (if memory-p
            (sdl2:set-texture-color-mod image 64 64 64)
            (sdl2:set-texture-color-mod image 255 255 255))
        (sdl2:render-copy *renderer* image
                          :dest-rect (sdl2:make-rect x y *tile-size* *tile-size*))))))

(defvar *key-action-map* (make-hash-table))

(defvar *state* :play)

(defun scancode-to-action (scancode)
  (and scancode
       (or (@ *key-action-map* *state* scancode)
           (and (eql *state* :inventory) (code-char scancode)))))

(defun true-key (key)
  (let* ((true-key (symbol-value (find-symbol (concatenate 'string
                                                           "+SDL-SCANCODE-"
                                                           (etypecase key
                                                             (symbol (symbol-name key))
                                                             (character (string-upcase key))
                                                             (cons (symbol-name (second key))))
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
      ;; (display-health (getf player-attributes :health)
      ;;                 (getf player-attributes :max-health)
      ;;                 (getf player-attributes :previous-health))
      ;; (display-stamina (getf player-attributes :stamina)
      ;;                  (getf player-attributes :max-stamina)
      ;;                  (getf player-attributes :previous-stamina))
      ;; (display-log 5 log)
      )))

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
             )))

(defun update-and-display (scancode)
  (multiple-value-bind (width height)
      (sdl2:get-window-size *window*)
    (destructuring-bind (state data) (rl:tick (scancode-to-action scancode))
      (setf *state* state)
      (ecase state
        (:play (draw-play data width height))
        (:inventory (draw-inventory data width height))))))

(defun dev ()
  (bt:make-thread (lambda () (main))
                  :name "game thread"))

(defun main ()
  (load-keys)
  (sdl2-image:init '(:png))
  (sdl2:with-init (:everything)
    (sdl2:with-window (*window* :flags '(:shown))
      (sdl2:with-renderer (*renderer* *window* :flags '(:accelerated))
        (init-textures)
        (rl:initialize)
        (update-and-display nil)

        (handler-case
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
                       (sdl2:gl-swap-window *window*))))
          (rl::quit-condition ()
      (sdl2-image:quit)))))))
