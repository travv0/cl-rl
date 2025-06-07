# Common Lisp Roguelike Code Review Report

## 1. Unidiomatic Common Lisp Code Patterns

### 1.1 Use of `defparameter` for Constants
- **Issue**: Using `defparameter` for values that should be constants (e.g., `*stage-width*`, `*stage-height*`, `*chunk-width*`, `*chunk-height*`, `*unarmed-damage*`, etc.)
- **Fix**: These should use `defconstant` or `alexandria:define-constant` since they don't change
- **Files**: level.lisp, collisions.lisp

### 1.2 Inconsistent Use of Special Variable Naming
- **Issue**: `*log*` is used as a special variable but manipulated with `push` which cons's new cells
- **Fix**: Consider using a more functional approach or a proper logging library
- **File**: rl.lisp

### 1.3 Non-standard Accessor Naming
- **Issue**: Using `%` prefix for slot names is uncommon in CL (more typical in Scheme)
- **Fix**: Use standard naming conventions without the prefix
- **Files**: All class definitions

## 2. Potential Bugs or Logic Errors

### 2.1 Race Condition in Chunk Loading
- **Issue**: `update-chunks` spawns a new thread every time without waiting for completion
- **Location**: level.lisp:142
- **Risk**: Multiple threads could be loading/unloading the same chunks simultaneously
- **Fix**: Use a thread pool or ensure previous operation completes before starting new one

### 2.2 Incomplete Error Handling in Marshal/Unmarshal
- **Issue**: `ms:unmarshal` operations in `load-chunk` have no error handling
- **Location**: level.lisp:120
- **Risk**: Corrupted save files will crash the game
- **Fix**: Add error handling and fallback behavior

### 2.3 Potential Division by Zero
- **Issue**: In `make-perlin-noise-seed`, parsing the denominator could result in zero
- **Location**: level.lisp:160
- **Risk**: Will cause arithmetic error
- **Fix**: Add explicit zero check before division

### 2.4 Stamina Update During Cooldown
- **Issue**: `cool-down` method in mixins.lisp:211-213 duplicates stamina regeneration logic already in `update :around` method
- **Risk**: Double stamina regeneration
- **Fix**: Remove duplicate logic or consolidate

## 3. Performance Issues

### 3.1 Inefficient Object Lookup
- **Issue**: `get-objects-at-pos` and related functions repeatedly check bounds
- **Location**: level.lisp:246-275
- **Fix**: Create a single bounds-checking function and reuse it

### 3.2 Repeated List Operations
- **Issue**: Using `remove-if`, `remove-if-not`, and `find-if` on the same list multiple times
- **Location**: Throughout level.lisp
- **Fix**: Use a single pass with `loop` to collect different object types

### 3.3 Hash Table Recreation
- **Issue**: `*name-cache*` grows indefinitely without cleanup
- **Location**: rl.lisp:23
- **Fix**: Add cache eviction or clear between levels

### 3.4 Expensive Line-of-Sight Calculations
- **Issue**: `update-can-see` performs many line calculations every frame
- **Location**: player.lisp:17-63
- **Fix**: Cache visibility results and only recalculate on movement

## 4. Code Duplication

### 4.1 Movement Direction Handling
- **Issue**: Repeated pattern for 8-directional movement in `tick` function
- **Location**: rl.lisp:168-207
- **Fix**: Use a lookup table or macro to reduce duplication

### 4.2 Similar Object Creation Functions
- **Issue**: `make-wall`, `make-grass`, `make-tree`, etc. all follow same pattern
- **Location**: level.lisp:221-244
- **Fix**: Create a generic factory function

## 5. Missing Error Handling

### 5.1 File I/O Operations
- **Issue**: No error handling for save/load operations
- **Location**: level.lisp:90-122
- **Fix**: Wrap in handler-case and provide user feedback

### 5.2 Thread Operations
- **Issue**: No error handling for thread creation
- **Location**: level.lisp:142
- **Fix**: Add error handling and fallback to synchronous operation

## 6. Improper Use of Special Variables or Global State

### 6.1 Direct Manipulation of Global State
- **Issue**: Functions directly modify `*game-objects*` and `*pos-cache*` without encapsulation
- **Location**: Throughout codebase
- **Fix**: Create proper accessor functions with validation

### 6.2 Thread-Unsafe Global Access
- **Issue**: `*chunk-lock*` only protects chunk operations, not `*game-objects*` access
- **Location**: level.lisp
- **Risk**: Race conditions when background thread loads chunks
- **Fix**: Extend locking to cover all shared state access

## 7. Memory Leaks or Resource Management Issues

### 7.1 Unbounded Cache Growth
- **Issue**: `*name-cache*` has no size limit or eviction policy
- **Location**: rl.lisp:23
- **Fix**: Implement LRU cache or clear between levels

### 7.2 Thread Leak
- **Issue**: Creating new threads without joining or managing them
- **Location**: level.lisp:142
- **Fix**: Use a thread pool or ensure threads complete

### 7.3 Position Cache Cleanup
- **Issue**: Objects removed from `*game-objects*` might still be in `*pos-cache*`
- **Location**: rl.lisp:234-238
- **Fix**: Ensure consistent cleanup of both data structures

## 8. Additional Issues

### 8.1 Magic Numbers
- **Issue**: Hard-coded values throughout (e.g., cooldown values, damage multipliers)
- **Fix**: Define as constants with meaningful names

### 8.2 Inconsistent nil/false Handling
- **Issue**: Some predicates return nil/t, others return nil/object
- **Fix**: Document and standardize return value conventions

### 8.3 Missing Type Declarations
- **Issue**: Performance-critical functions lack type declarations
- **Location**: pos.lisp distance calculations, collision detection
- **Fix**: Add appropriate `declare` forms

### 8.4 Circular Dependencies
- **Issue**: Complex interdependencies between files could cause load order issues
- **Fix**: Review and restructure module dependencies

## Recommendations

1. **Immediate fixes needed**:
   - Thread safety issues in chunk loading
   - Error handling for file I/O
   - Memory leak in name cache

2. **Medium priority**:
   - Performance optimizations for visibility calculations
   - Code duplication reduction
   - Proper constant definitions

3. **Long-term improvements**:
   - Refactor global state management
   - Implement proper logging system
   - Add comprehensive error handling throughout