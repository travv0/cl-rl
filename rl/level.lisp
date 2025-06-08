(in-package #:rl)

(defvar *pos-cache*)
(defvar *game-objects*)

(defun ensure-data-directories ()
  "Ensure required data directories exist. Should be called at game startup."
  (handler-case
      (progn
        (ensure-directories-exist "data/chunks/")
        t)
    (error (e)
      (write-to-log "Failed to create data directories: ~a" e)
      nil)))

(define-class terrain ()
  ((%cooldown-modifier :initarg :cooldown-modifier :initform 1 :accessor cooldown-modifier)))

(define-class spawn (pos)
  ())

(define-class wall (visible solid opaque)
  ())

(define-class grass (terrain visible)
  ())

(define-class tree (visible solid opaque)
  ())

(define-class water (visible solid)
  ())

(define-class shallow-water (terrain visible)
  ((%cooldown-modifier :initform 1.5)))

(define-class sand (terrain visible)
  ((%cooldown-modifier :initform 1.2)))

(define-class door (visible)
  ())

(defmethod should-display (obj)
  nil)

(defmethod should-display ((obj visible))
  (can-see obj))

(defparameter *stage-width* 200)
(defparameter *stage-height* 200)

(defparameter *chunk-width* 40)
(defparameter *chunk-height* 40)

(defun player-chunk ()
  (with-accessors ((x x) (y y)) *player*
    (pos (* (floor x *chunk-width*) *chunk-width*)
         (* (floor y *chunk-height*) *chunk-height*))))

(defun chunks-to-show ()
  (let ((player-chunk (player-chunk))
        (diffs (list (list (- *chunk-width*) (- *chunk-height*))
                     (list 0 (- *chunk-height*))
                     (list *chunk-width* (- *chunk-height*))
                     (list *chunk-width* 0)
                     (list *chunk-width* *chunk-height*)
                     (list 0 *chunk-height*)
                     (list (- *chunk-width*) *chunk-height*)
                     (list (- *chunk-width*) 0)
                     (list 0 0))))
    (loop for diff in diffs
          for p = (add player-chunk (to-pos diff))
          when (and (<= 0 (x p) (1- *stage-width*))
                    (<= 0 (y p) (1- *stage-height*)))
            collect p)))

(defun all-chunks ()
  (loop for y from 0 below *stage-height* by *chunk-height*
        nconc (loop for x from 0 below *stage-width* by *chunk-width*
                    collect (pos x y))))

(defun chunk-range-to-show ()
  (let ((positions (chunks-to-show)))
    (loop for p in positions
          minimizing (x p) into min-x
          minimizing (y p) into min-y
          maximizing (+ (x p) *chunk-width*) into max-x
          maximizing (+ (y p) *chunk-width*) into max-y
          finally (return (values min-x min-y max-x max-y)))))

(defun chunks-to-unload ()
  (loop for chunk in (all-chunks)
        unless (member chunk (chunks-to-show) :test #'pos-equal)
          collect chunk))

(defun save-and-unload-chunk (chunk-pos)
  "Save and unload a chunk. Must be called with *game-state-lock* held."
  (when (%save-chunk chunk-pos)
    ;; Only unload if save succeeded
    (unload-chunk chunk-pos)))

(defun %save-chunk (chunk-pos)
  "Internal version of save-chunk. Requires *game-state-lock* to be held."
  (with-accessors ((chunk-x x) (chunk-y y)) chunk-pos
    (let ((filename (format nil "data/chunks/~d_~d" chunk-x chunk-y)))
      (handler-case
          (progn
            ;; Ensure directory exists
            (ensure-directories-exist "data/chunks/")
            (with-standard-io-syntax
              (with-output-to-file (s filename
                                      :if-exists :overwrite
                                      :if-does-not-exist :create)
                (prin1 (ms:marshal (loop for cy from chunk-y below (min (+ chunk-y *chunk-height*) *stage-height*)
                                         nconc (loop for cx from chunk-x below (min (+ chunk-x *chunk-width*) *stage-width*)
                                                     nconc (reverse (aref *pos-cache* cx cy)))))
                       s))
            ;; Return t to indicate success
            t))
        (error (e)
          (write-to-log "Failed to save chunk ~d,~d: ~a" chunk-x chunk-y e)
          ;; Return nil to indicate failure
          nil)))))

(defun save-chunk (chunk-pos)
  "Save chunk with chunk-pos being its top-left corner to disk. Thread-safe."
  (bt:with-lock-held (*game-state-lock*)
    (%save-chunk chunk-pos)))

(defun save-world ()
  "Save all chunks of world to disk. Returns number of chunks successfully saved."
  (let ((saved 0)
        (failed 0))
    (loop for chunk in (all-chunks)
          do (if (save-chunk chunk)
                 (incf saved)
                 (incf failed)))
    (when (> failed 0)
      (write-to-log "Failed to save ~d chunks" failed))
    saved))

(defun unload-world ()
  "clear all game objects"
  (clear-objects))

(defun unload-chunk (chunk-pos)
  (with-accessors ((chunk-x x) (chunk-y y)) chunk-pos
    (loop for cy from chunk-y below (min (+ chunk-y *chunk-height*) *stage-height*) do
      (loop for cx from chunk-x below (min (+ chunk-x *chunk-width*) *stage-width*) do
        (%clear-position (pos cx cy))))))

(defun load-chunk (chunk-pos)
  "Load a chunk from disk. Handles errors gracefully."
  (with-accessors ((chunk-x x) (chunk-y y)) chunk-pos
    (let ((filename (format nil "data/chunks/~d_~d" chunk-x chunk-y)))
      (handler-case
          (with-standard-input-syntax
            (with-input-from-file (s filename :if-does-not-exist nil)
              (when s
                (let ((objs (ms:unmarshal (read s))))
                  (loop for obj in objs do
                    (%add-object obj))))))
        (error (e)
          (write-to-log "Failed to load chunk ~d,~d: ~a" chunk-x chunk-y e)
          ;; Continue without the chunk - it will be regenerated if needed
          nil)))))

(defun ensure-chunks-loaded (chunk-positions)
  "Load chunks that aren't already loaded. Must be called with lock held."
  (loop for p in chunk-positions
        unless (aref *pos-cache* (x p) (y p))
          do (load-chunk p)))

(defun ensure-chunks-unloaded (chunk-positions)
  "Save and unload chunks that are loaded. Must be called with lock held."
  (loop for p in chunk-positions
        when (aref *pos-cache* (x p) (y p))
          do (save-and-unload-chunk p)))

(defvar *chunk-loader-thread* nil
  "Thread for loading/unloading chunks in the background")

;;; Thread-safe accessor functions

(defun safe-get-pos-cache-at (x y)
  "Thread-safe read from pos-cache"
  (bt:with-lock-held (*game-state-lock*)
    (aref *pos-cache* x y)))

(defun safe-set-pos-cache-at (x y value)
  "Thread-safe write to pos-cache"
  (bt:with-lock-held (*game-state-lock*)
    (setf (aref *pos-cache* x y) value)))

(defun safe-push-to-pos-cache (obj x y)
  "Thread-safe push to pos-cache"
  (bt:with-lock-held (*game-state-lock*)
    (push obj (aref *pos-cache* x y))))

(defun safe-remove-from-pos-cache (obj x y)
  "Thread-safe remove from pos-cache"
  (bt:with-lock-held (*game-state-lock*)
    (removef (aref *pos-cache* x y) obj)))

(defun safe-push-to-game-objects (obj)
  "Thread-safe push to game-objects"
  (bt:with-lock-held (*game-state-lock*)
    (push obj *game-objects*)))

(defun safe-remove-from-game-objects (obj)
  "Thread-safe remove from game-objects"
  (bt:with-lock-held (*game-state-lock*)
    (removef *game-objects* obj)))

(defun safe-clear-game-state ()
  "Thread-safe clear of game state"
  (bt:with-lock-held (*game-state-lock*)
    (setf *game-objects* '())
    (setf *pos-cache* (make-array (list *stage-width* *stage-height*)
                                  :element-type 'list
                                  :initial-element '()))))

;; Internal versions that assume lock is already held
(defun %push-to-game-objects (obj)
  "Internal version - requires lock to be held"
  (push obj *game-objects*))

(defun %push-to-pos-cache (obj x y)
  "Internal version - requires lock to be held"
  (push obj (aref *pos-cache* x y)))

(defun %add-object (obj)
  "Internal version of add-object - requires lock to be held"
  (%push-to-game-objects obj)
  (when (typep obj 'pos)
    (%push-to-pos-cache obj (x obj) (y obj)))
  obj)

(defun %remove-from-game-objects (obj)
  "Internal version - requires lock to be held"
  (removef *game-objects* obj))

(defun %clear-position (pos)
  "Internal version of clear-position - requires lock to be held"
  (loop for obj in (remove-if-not (op (typep _ 'visible))
                                  (aref *pos-cache* (x pos) (y pos)))
        do (%remove-from-game-objects obj))
  (setf (aref *pos-cache* (x pos) (y pos)) '()))

(defun ensure-chunks ()
  (bt:with-lock-held (*game-state-lock*)
    (ensure-chunks-loaded (chunks-to-show))
    (ensure-chunks-unloaded (chunks-to-unload))))

(defun update-chunks ()
  "Update chunks in a background thread. Reuses existing thread if still running."
  (when (or (null *chunk-loader-thread*)
            (not (bt:thread-alive-p *chunk-loader-thread*)))
    (setf *chunk-loader-thread* 
          (bt:make-thread #'ensure-chunks 
                          :name "chunk-loader"))))

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
  (let* ((string-seed (write-to-string seed))
         (len (length string-seed)))
    (if (< len 4)
        ;; If seed is too short, just use it directly
        (/ seed 1000.0)
        (let* ((numerator (subseq string-seed 0 (- len 3)))
               (denominator (subseq string-seed (- len 3))))
          (handler-case 
              (let ((num (parse-integer numerator))
                    (denom (parse-integer denominator)))
                ;; Ensure denominator is never zero
                (if (zerop denom)
                    (/ num 1000.0)
                    (/ num denom)))
            (error () 
              ;; If parsing fails, fall back to simple conversion
              (/ seed 1000.0)))))))

(defun make-secret-entrance ()
  (let ((tree (random-elt (remove-if-not (lambda (obj) (and (typep obj 'tree)
                                                            (< 0 (x obj) (1- *stage-width*))
                                                            (< 0 (y obj) (1- *stage-height*))))
                                         *game-objects*)))
        (directions (remove-duplicates
                     (tu:combinations '(-1 -1 0 1 1) 2)
                     :test #'equal)))
    (clear-position tree)
    (add-object (make-water (x tree) (y tree) :shallow t))
    (loop for (dx dy) in (remove (random-elt directions)
                                 directions
                                 :test #'equal)
          do (add-object (make-tree (+ (x tree) dx)
                                    (+ (y tree) dy))))
    (pos (x tree) (y tree))))

(defun init-grass-area (width height seed)
  (flet ((retry ()
           (clear-objects)
           (init-grass-area width height (random 10000000))
           (return-from init-grass-area)))
    (let ((seed (if (integerp seed)
                    (make-perlin-noise-seed seed)
                    seed)))
      (loop for y from (1- height) downto 0 do
        (loop for x below width do
          (let* ((noise (grass-area-noise x y seed)))
            (cond ((< noise -0.02) (add-object (make-water x y)))
                  ((< noise -0.0) (add-object (make-water x y :shallow t)))
                  ((< noise 0.02) (add-object (make-sand x y)))
                  (t
                   (add-object (make-grass x y))
                   (when (> noise 0.1)
                     (let ((tree-noise (tree-noise x y seed)))
                       (when (> tree-noise 30)
                         (add-object (make-tree x y))))))))))

      (unless (and (> (count-if (op (typep _ 'water)) *game-objects*) 700)
                   (> (count-if (op (typep _ 'tree)) *game-objects*) 300))
        (retry))

      (let* ((secret-entrance (make-secret-entrance))
             (spawn (add-object (make-spawn))))
        (unless (find-path spawn secret-entrance)
          (retry))))))

(defun init-lava-area (x y width height seed)
  (let* ((noise (lava-area-noise x y seed)))
    (cond ((< noise -0.2) (add-object (make-water x y)))
          (t (let ((rock-noise (tree-noise x y seed)))
               (when (> rock-noise 40)
                 (add-object (make-tree x y))))))))

(defun init-floor (width height &optional (seed 0))
  (init-grass-area width height seed))

(defun make-wall (x y)
  (make-instance 'wall :x x :y y))

(defun make-grass (x y)
  (make-instance 'grass :x x :y y))

(defun make-tree (x y)
  (make-instance 'tree :x x :y y))

(defun make-water (x y &key shallow)
  (if shallow
      (make-instance 'shallow-water :x x :y y)
      (make-instance 'water :x x :y y)))

(defun make-sand (x y)
  (make-instance 'sand :x x :y y))

(defun make-spawn ()
  (let ((p (random-pos)))
    (make-instance 'spawn :x (x p) :y (y p))))

(defun make-door (x y)
  (make-instance (mix 'opaque 'solid 'door) :x x :y y))

(defun get-objects-at-pos (pos)
  "Returns all visible objects at the given position. Thread-safe."
  (when (and (<= 0 (x pos) (1- *stage-width*))
             (<= 0 (y pos) (1- *stage-height*)))
    (remove-if-not (op (typep _ 'visible))
                   (safe-get-pos-cache-at (x pos) (y pos)))))

(defun get-object-at-pos (pos)
  "Returns the first visible object at the given position. Thread-safe."
  (when (and (<= 0 (x pos) (1- *stage-width*))
             (<= 0 (y pos) (1- *stage-height*)))
    (find-if (op (typep _ 'visible))
             (safe-get-pos-cache-at (x pos) (y pos)))))

(defun get-visible-objects-at-pos (pos)
  "Returns all objects that should be displayed at the given position. Thread-safe."
  (declare (optimize speed))
  (when (and (<= 0 (x pos) (1- *stage-width*))
             (<= 0 (y pos) (1- *stage-height*)))
    (remove-if-not #'should-display 
                   (safe-get-pos-cache-at (x pos) (y pos)))))

(defun get-visible-object-at-pos (pos)
  (when (and (<= 0 (x pos) (1- *stage-width*))
             (<= 0 (y pos) (1- *stage-height*)))
    (find-if #'should-display 
             (safe-get-pos-cache-at (x pos) (y pos)))))

(defun terrain-p (pos)
  (typep pos 'terrain))

(defun get-terrain-at-pos (pos)
  (when (and (<= 0 (x pos) (1- *stage-width*))
             (<= 0 (y pos) (1- *stage-height*)))
    (find-if #'terrain-p 
             (safe-get-pos-cache-at (x pos) (y pos)))))

(defun random-pos ()
  (loop for x = (random *stage-width*)
        for y = (random *stage-height*)
        unless (member-if (op (typep _ 'solid)) (get-objects-at-pos (pos x y)))
          do (return (pos x y))))
