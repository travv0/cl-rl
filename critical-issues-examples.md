# Critical Issues with Code Examples

## 1. Thread Safety Issue in Chunk Loading

**Current problematic code** (level.lisp:135-142):
```lisp
(defvar *chunk-lock* (bt:make-lock))

(defun ensure-chunks ()
  (bt:with-lock-held (*chunk-lock*)
    (ensure-chunks-loaded (chunks-to-show))
    (ensure-chunks-unloaded (chunks-to-unload))))

(defun update-chunks ()
  (bt:make-thread #'ensure-chunks))
```

**Problems**:
1. New thread created every call without tracking
2. Lock only protects chunk operations, not `*game-objects*` modifications
3. No synchronization with main game loop

**Suggested fix**:
```lisp
(defvar *chunk-lock* (bt:make-lock))
(defvar *chunk-thread* nil)
(defvar *chunk-thread-lock* (bt:make-lock))

(defun update-chunks ()
  (bt:with-lock-held (*chunk-thread-lock*)
    (when (and *chunk-thread* (bt:thread-alive-p *chunk-thread*))
      (return-from update-chunks))  ; Previous operation still running
    (setf *chunk-thread*
          (bt:make-thread 
            (lambda ()
              (handler-case
                  (ensure-chunks)
                (error (e)
                  (format *error-output* "Chunk loading error: ~A~%" e))))
            :name "chunk-loader"))))
```

## 2. Double Stamina Regeneration Bug

**Current problematic code** (mixins.lisp):
```lisp
;; In update :around method for alive (line 200-205)
(defmethod update :around ((obj alive))
  (let ((start-stamina (stamina obj)))
    (call-next-method)
    (unless (< (stamina obj) start-stamina)
      (setf (stamina obj) (min (+ (stamina obj) 1) (max-stamina obj))))))

;; In cool-down method (line 209-214)
(defmethod cool-down ((obj cooldown))
  (when (plusp (cooldown obj))
    (let ((start-stamina (stamina obj)))
      (unless (< (stamina obj) start-stamina)
        (setf (stamina obj) (min (+ (stamina obj) 1) (max-stamina obj)))))
    (decf (cooldown obj))))
```

**Problem**: Both methods regenerate stamina, potentially giving 2 stamina per tick.

**Fix**: Remove stamina regeneration from `cool-down`:
```lisp
(defmethod cool-down ((obj cooldown))
  (when (plusp (cooldown obj))
    (decf (cooldown obj))))
```

## 3. Division by Zero in Perlin Noise Seed

**Current problematic code** (level.lisp:157-162):
```lisp
(defun make-perlin-noise-seed (seed)
  (let* ((string-seed (write-to-string seed))
         (numerator (subseq string-seed 0 (- (length string-seed) 3)))
         (denominator (subseq string-seed (- (length string-seed) 3))))
    (handler-case (/ (parse-integer numerator) (parse-integer denominator))
      (arithmetic-error () (make-perlin-noise-seed (+ seed 9))))))
```

**Problems**:
1. If seed has fewer than 3 digits, `subseq` will fail
2. Denominator could be "000", causing division by zero
3. Recursive call could stack overflow

**Fix**:
```lisp
(defun make-perlin-noise-seed (seed)
  (let* ((string-seed (format nil "~7,'0D" seed))  ; Ensure 7 digits
         (numerator (parse-integer (subseq string-seed 0 4)))
         (denominator (parse-integer (subseq string-seed 4))))
    (when (zerop denominator)
      (setf denominator 1))
    (/ numerator denominator)))
```

## 4. Memory Leak in Name Cache

**Current problematic code** (rl.lisp:23-34):
```lisp
(defparameter *name-cache* (make-hash-table))

(defmethod display-name (obj)
  (if-let ((name (gethash obj *name-cache*)))
    name
    (flet ((format-name (class)
             (substitute #\space #\- (string-downcase (class-name class)))))
      (let ((obj-name (format-name (primary-class-of-mixin obj)))
            (obj-modifiers (mapcar (op (format-name _))
                                   (get-modifiers obj))))
        (setf (gethash obj *name-cache*)
              (format nil "~{~a ~}~a" obj-modifiers obj-name))))))
```

**Problem**: Cache grows indefinitely, holding references to deleted objects.

**Fix**:
```lisp
(defparameter *name-cache* (make-hash-table :test 'eq :weakness :key))

;; Or add explicit cleanup:
(defun clear-name-cache ()
  (clrhash *name-cache*))

;; Call in initialize function
(defun initialize (&optional seed)
  (clear-name-cache)
  ;; ... rest of initialization
  )
```

## 5. Race Condition in Position Cache

**Current problematic code** (pos.lisp:96-105):
```lisp
(defmethod update-pos ((obj pos) new-x new-y)
  "update the position of an object. use this instead of setting
object's x and y coordinates directly."
  (let ((new-x (clamp new-x 0 (1- *stage-width*)))
        (new-y (clamp new-y 0 (1- *stage-height*))))
    (with-accessors ((x x) (y y)) obj
      (removef (aref *pos-cache* x y) obj)
      (push obj (aref *pos-cache* new-x new-y))
      (setf (slot-value obj '%x) new-x
            (slot-value obj '%y) new-y))))
```

**Problem**: No locking when background thread might be accessing `*pos-cache*`.

**Fix**:
```lisp
(defvar *pos-cache-lock* (bt:make-lock))

(defmethod update-pos ((obj pos) new-x new-y)
  (let ((new-x (clamp new-x 0 (1- *stage-width*)))
        (new-y (clamp new-y 0 (1- *stage-height*))))
    (bt:with-lock-held (*pos-cache-lock*)
      (with-accessors ((x x) (y y)) obj
        (removef (aref *pos-cache* x y) obj)
        (push obj (aref *pos-cache* new-x new-y))
        (setf (slot-value obj '%x) new-x
              (slot-value obj '%y) new-y)))))
```

## 6. Missing Error Handling in File I/O

**Current problematic code** (level.lisp:116-122):
```lisp
(defun load-chunk (chunk-pos)
  (with-accessors ((chunk-x x) (chunk-y y)) chunk-pos
    (with-standard-input-syntax
      (with-input-from-file (s (format nil "data/chunks/~d_~d" chunk-x chunk-y))
        (let ((objs (ms:unmarshal (read s))))
          (loop for obj in objs do
            (add-object obj)))))))
```

**Problems**:
1. No check if file exists
2. No error handling for corrupted data
3. No cleanup if error occurs mid-load

**Fix**:
```lisp
(defun load-chunk (chunk-pos)
  (with-accessors ((chunk-x x) (chunk-y y)) chunk-pos
    (let ((filename (format nil "data/chunks/~d_~d" chunk-x chunk-y)))
      (handler-case
          (when (probe-file filename)
            (with-standard-input-syntax
              (with-input-from-file (s filename)
                (let ((objs (ms:unmarshal (read s))))
                  (loop for obj in objs do
                    (add-object obj))))))
        (error (e)
          (warn "Failed to load chunk ~A,~A: ~A" chunk-x chunk-y e)
          ;; Generate chunk procedurally as fallback
          nil)))))
```