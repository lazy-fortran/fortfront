# Windows Heap Corruption Investigation

## Issue
CI tests fail on Windows with exit code -1073740940 (0xC0000374) indicating heap corruption.

## Investigation Log

### Suspected Areas
1. **ast_arena_pop** - Recent implementation with deallocation logic
2. **Assignment operators** - Recently modified for AST types
3. **Child indices management** - Dynamic array operations

### Windows-Specific Considerations
- Different memory alignment requirements
- Stricter heap validation
- Potential issues with MSYS2/MinGW runtime

## Root Causes Found & Fixed

### 1. Array Bounds Violation in ast_arena_pop
**Problem**: When removing a child from parent's child_indices array, the code would access out-of-bounds memory if removing the last element.
**Fix**: Added bounds check before array shifting operations.

### 2. Shallow Copy in ast_arena_assign
**Problem**: Direct array assignment caused double-free errors when both objects were destroyed.
**Fix**: Implemented proper deep copy using element-wise assignment.

### 3. Stale Memory Access in Child Indices
**Problem**: After removing a child, the array wasn't resized, leaving stale indices.
**Fix**: Properly resize arrays after removal or deallocate if empty.

### 4. Missing Defensive Checks
**Problem**: No validation of array bounds and indices before operations.
**Fix**: Added defensive checks for valid indices and circular references.

### 5. Uninitialized Memory in get_node Function
**Problem**: The get_node function in fortfront.f90 used `allocate(node, mold=...)` which only allocated memory but didn't copy data, leaving derived type fields uninitialized.
**Fix**: Changed to use `allocate(node, source=...)` for proper deep copy.

### 6. Incomplete Assignment Operators
**Problem**: AST node assignment operators didn't copy base class fields (line, column, inferred_type), causing uninitialized memory when `allocate(source=)` used the assignment operator.
**Fix**: Updated all assignment operators to properly copy base class fields before derived class fields.

### 7. Code Review Improvements (QODO)
**Problems identified by QODO**:
- Overly restrictive array shifting condition that would skip necessary shifts
- Corruption detection that silently reset state instead of returning early
- Complex array resizing logic using temporary arrays and move_alloc

**Fixes**:
- Simplified array shifting condition to only check if elements need shifting
- Changed corruption detection to return early for safety
- Used Fortran array extension syntax per CLAUDE.md policy for cleaner code
- Kept arrays as-is when children remain, only deallocate when empty

## Status
**RESOLVED** - All fixes have been implemented, tested, and code review suggestions incorporated.