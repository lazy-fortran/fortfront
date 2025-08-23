# Development Backlog

## DOING (Current Work)
- [ ] #369: feat: define base arena interface with type-bound procedures (branch: feat/base-arena-interface-369)

## TODO (Ordered by Priority)

### Foundation Phase 1: Error Handling Infrastructure (CRITICAL)
No remaining items - Phase 1 complete

### Foundation Phase 2: Arena System (FOUNDATION)
- [ ] #370: feat: migrate ast_arena to container API with type-bound procedures
- [ ] #362: Create unified compiler_arena module for KISS architecture
- [ ] #371: feat: integrate compiler_arena for unified memory management
- [ ] #360: Migrate AST to modern high-performance arena with unified architecture

### Foundation Phase 3: CST/AST Infrastructure (MAJOR)
- [ ] #393: CST: Create basic CST node type definitions and module structure
- [ ] #394: CST: Implement UID generation system for stable node identification
- [ ] #395: CST: Implement lexer trivia collection for comments and whitespace
- [ ] #396: CST: Create CST builder for parallel construction alongside AST
- [ ] #397: CST: Implement CST to AST converter with bidirectional linking

### Code Quality and Refactoring (ENHANCEMENT)
- [ ] #407: Refactor: Extract multi-variable declaration handling from parse_declaration (Re-opened after regression fixes needed)
- [ ] #406: Refactor: Extract variable parsing and initialization logic from parse_declaration
- [ ] #364: refactor: break down parse_declaration function (519 lines -> <100 lines)
- [ ] #365: refactor: break down large functions >200 lines (9 functions)
- [ ] #366: refactor: address remaining 24 functions exceeding 100-line limit
- [ ] #367: refactor: address remaining large files >1000 lines

### Type System Improvements (ENHANCEMENT)
- [ ] #401: Type System: Implement constraint generation for Hindley-Milner type inference
- [ ] #402: Type System: Implement unification algorithm with occurs check for constraint solving
- [ ] #403: Type System: Implement constraint solver with let-polymorphism support

### Testing and Documentation (SUPPORT)
- [ ] #361: Create GCC Bug 114612 test suite and permanent regression prevention

## DONE (Completed)
- [x] #410: Error Handling: Migrate parser to use unified result_t with comprehensive error recovery (PR #434)
- [x] #400: Arena: Create comprehensive performance benchmark suite for arena operations (PR #436)
- [x] #399: Arena: Implement per-node freeing with generation tracking for selective memory management (PR #435)
- [x] #359: Implement arena memory allocator with generation-based safety (Comprehensive implementation complete)
- [x] #408: Error Handling: Design and implement unified result_t type for consistent error handling (PR #432)
- [x] #427: Error Handling: Migrate lexer to unified result_t error handling
