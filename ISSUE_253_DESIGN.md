# Fix Issue #253: Module parsing not fully supported

This branch will fix the module parsing bug where functions inside modules
are incorrectly moved outside the module structure and wrapped in programs.

## Root Cause
The standardize_module() function calls standardize_ast() on module procedures,
which treats them as standalone functions and wraps them in programs.

## Solution
Add context-aware standardization to skip program wrapping when inside modules.

