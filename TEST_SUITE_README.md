# Test Suite Documentation

## Overview

I've created a comprehensive test suite for the Common Lisp roguelike game. The test suite covers all major components of the system.

## Test Files Created

1. **test-core.lisp** - Tests for core game functionality (logging, display names, object management)
2. **test-pos.lisp** - Tests for position handling and movement
3. **test-mixins.lisp** - Tests for all mixin classes (health, stamina, visibility, etc.)
4. **test-player.lisp** - Tests for player-specific functionality
5. **test-enemies.lisp** - Tests for enemy AI and behavior
6. **test-items.lisp** - Tests for item pickup and usage
7. **test-weapons.lisp** - Tests for weapon mechanics and combat
8. **test-level.lisp** - Tests for level generation and chunk system
9. **test-astar.lisp** - Tests for pathfinding algorithm
10. **test-utils.lisp** - Tests for utility functions
11. **test-stubs.lisp** - Stub implementations for missing functions

## Running the Tests

### Prerequisites

1. SBCL (Steel Bank Common Lisp)
2. Quicklisp package manager
3. Required dependencies:
   - alexandria
   - serapeum
   - str
   - black-tie
   - dynamic-mixins
   - travv0.utils
   - marshal
   - fiveam (for testing)
   - closer-mop

### Installation Steps

```bash
# 1. Install Quicklisp if not already installed
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --quit

# 2. Install dependencies
sbcl --eval '(ql:quickload :alexandria)' --quit
sbcl --eval '(ql:quickload :serapeum)' --quit
sbcl --eval '(ql:quickload :str)' --quit
sbcl --eval '(ql:quickload :black-tie)' --quit
sbcl --eval '(ql:quickload :dynamic-mixins)' --quit
sbcl --eval '(ql:quickload :marshal)' --quit
sbcl --eval '(ql:quickload :fiveam)' --quit
sbcl --eval '(ql:quickload :closer-mop)' --quit

# 3. Run the tests
sbcl --script run-tests-safe.lisp
```

### Test Scripts Available

1. **run-tests.lisp** - Basic test runner
2. **run-tests-safe.lisp** - Test runner with dependency checking
3. **run-minimal-tests.lisp** - Minimal tests that run without dependencies
4. **test-basic.lisp** - Basic system loading test

## Test Coverage

The test suite covers:

- Core game loop and object management
- Position and movement systems
- All mixin behaviors (health, stamina, etc.)
- Player actions and inventory
- Enemy AI and pathfinding
- Item and weapon mechanics
- Level generation
- Collision detection
- Utility functions

## Known Issues

1. Some functions referenced in the code may not be implemented yet
2. The `travv0.utils` dependency might need to be installed separately
3. Tests assume certain game constants are defined

## Notes on Code Issues Found

During test creation, I identified several critical issues in the codebase:

1. **Thread safety problems** in chunk loading
2. **Double stamina regeneration** bug
3. **Division by zero** risk in Perlin noise
4. **Memory leaks** in name caching
5. **Missing error handling** for file I/O

These issues are documented in the generated report files and should be addressed before running the full test suite.