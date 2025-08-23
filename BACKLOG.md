# Development Backlog

## TODO (Ordered by Priority)
- [ ] #359: Implement arena memory allocator with generation-based safety
- [ ] #369: Define base arena interface with type-bound procedures
- [ ] #370: Migrate ast_arena to container API with type-bound procedures
- [ ] #398: Arena: Implement arena handle validation with generation checking
- [ ] #399: Arena: Implement per-node freeing with generation tracking for selective memory management
- [ ] #329: Fix character type inference for string literals and expressions
- [ ] #354: GCC 15.2.1 compatibility issue with allocatable components
- [ ] #406: Refactor: Extract variable parsing and initialization logic from parse_declaration
- [ ] #393: CST: Create basic CST node type definitions and module structure
- [ ] #394: CST: Implement UID generation system for stable node identification
- [ ] #395: CST: Implement lexer trivia collection for comments and whitespace
- [ ] #396: CST: Create CST builder for parallel construction alongside AST
- [ ] #397: CST: Implement CST to AST converter with bidirectional linking
- [ ] #401: Type System: Implement constraint generation for Hindley-Milner type inference
- [ ] #402: Type System: Implement unification algorithm with occurs check for constraint solving
- [ ] #403: Type System: Implement constraint solver with let-polymorphism support
- [ ] #400: Arena: Create comprehensive performance benchmark suite for arena operations

## DOING (Current Work)
- [x] #410: Error Handling: Migrate parser to use unified result_t with comprehensive error recovery (branch: feat/parser-result-t-migration-410)

## DONE (Completed)
- [x] #408: Error Handling: Design and implement unified result_t type for consistent error handling
- [x] #407: Refactor: extract multi-variable declaration handling from parse_declaration (PR #429)
- [x] #428: Refactor: extract type specifier parsing from parse_declaration function
- [x] #427: Feat: migrate lexer to unified result_t error handling
- [x] #426: Arch: simplify foundation to FPM-first approach
- [x] #424: Feat: Foundation static library architecture for pure Fortran integration