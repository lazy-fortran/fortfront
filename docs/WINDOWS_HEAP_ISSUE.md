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

## Next Steps
1. Add defensive checks in deallocation routines
2. Verify all allocatable components are properly initialized
3. Check for array bounds violations
4. Consider using nullify() after deallocate()