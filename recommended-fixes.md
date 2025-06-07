# Recommended Fixes for CL-RL Codebase

## Priority 1: Critical Bugs (Fix Immediately)

### 1. Thread Safety in Chunk Loading
**File**: level.lisp
**Issue**: Race conditions between main thread and chunk loading thread
**Fix**:
```lisp
;; Add at top of level.lisp
(defvar *game-state-lock* (bt:make-lock "game-state-lock"))
(defvar *chunk-loader-thread* nil)

;; Wrap all *game-objects* and *pos-cache* access with:
(bt:with-lock-held (*game-state-lock*)
  ;; ... access code here
  )
```

### 2. Double Stamina Regeneration
**File**: mixins.lisp, line 211-213
**Issue**: Stamina regenerates in both `update` and `cool-down`
**Fix**: Remove stamina regeneration from `cool-down` method

### 3. Division by Zero Risk
**File**: level.lisp, line 157-162
**Issue**: Perlin noise seed generation can divide by zero
**Fix**: Add validation and ensure non-zero denominator

## Priority 2: Memory and Resource Leaks

### 1. Unbounded Name Cache
**File**: rl.lisp
**Fix**: Use weak hash table or add cache clearing:
```lisp
(defparameter *name-cache* (make-hash-table :test 'eq :weakness :key))
```

### 2. Thread Leak
**File**: level.lisp
**Fix**: Track and reuse threads instead of creating new ones

## Priority 3: Performance Improvements

### 1. Visibility Calculations
**File**: player.lisp
**Issue**: Recalculates entire visibility every frame
**Fix**: Cache visibility and only update on player movement:
```lisp
(defvar *visibility-cache* (make-hash-table :test 'equal))
(defvar *last-player-pos* nil)

(defun update-can-see (from)
  (let ((current-pos (cons (x from) (y from))))
    (when (not (equal current-pos *last-player-pos*))
      (clrhash *visibility-cache*)
      (setf *last-player-pos* current-pos)
      ;; ... existing visibility calculation
      )))
```

### 2. Object Lookup Optimization
**File**: level.lisp
**Fix**: Create unified bounds-checking:
```lisp
(defun valid-pos-p (x y)
  (and (<= 0 x (1- *stage-width*))
       (<= 0 y (1- *stage-height*))))

(defun get-objects-at-pos (pos)
  (when (valid-pos-p (x pos) (y pos))
    (remove-if-not (op (typep _ 'visible))
                   (aref *pos-cache* (x pos) (y pos)))))
```

## Priority 4: Code Quality Improvements

### 1. Replace defparameter with Constants
**Files**: level.lisp, collisions.lisp
```lisp
;; Change from:
(defparameter *stage-width* 100)
;; To:
(alexandria:define-constant +stage-width+ 100)
```

### 2. Error Handling for I/O
**File**: level.lisp
**Add error handling to all file operations**:
```lisp
(defun safe-save-chunk (chunk-pos)
  (handler-case
      (save-chunk chunk-pos)
    (error (e)
      (warn "Failed to save chunk ~A: ~A" chunk-pos e))))
```

### 3. Reduce Code Duplication
**File**: rl.lisp, lines 168-207
**Create movement direction table**:
```lisp
(defparameter *movement-actions*
  '((:move-left -1 0 nil)
    (:move-right 1 0 nil)
    (:move-up 0 -1 nil)
    (:move-down 0 1 nil)
    (:move-up-left -1 -1 nil)
    (:move-up-right 1 -1 nil)
    (:move-down-left -1 1 nil)
    (:move-down-right 1 1 nil)
    (:run-left -1 0 t)
    (:run-right 1 0 t)
    (:run-up 0 -1 t)
    (:run-down 0 1 t)
    (:run-up-left -1 -1 t)
    (:run-up-right 1 -1 t)
    (:run-down-left -1 1 t)
    (:run-down-right 1 1 t)))

;; Then in tick function:
(let ((movement (assoc action *movement-actions*)))
  (when movement
    (destructuring-bind (action dx dy running-p) movement
      (when running-p
        (ensure-mix *player* 'running))
      (setf (dx *player*) dx
            (dy *player*) dy)
      t)))
```

## Testing Recommendations

1. Add tests for thread safety
2. Add tests for error conditions (corrupted saves, etc.)
3. Add performance benchmarks for visibility calculations
4. Add memory usage tests to catch leaks

## Implementation Order

1. **Week 1**: Fix critical bugs (thread safety, double stamina, division by zero)
2. **Week 2**: Fix memory leaks and add error handling
3. **Week 3**: Performance optimizations
4. **Week 4**: Code quality improvements and refactoring

## Additional Recommendations

1. Consider using a proper logging library instead of `*log*` list
2. Add type declarations for performance-critical functions
3. Document thread safety requirements for each function
4. Consider using CLOS method combinations to reduce code duplication
5. Add a style guide for consistent coding conventions