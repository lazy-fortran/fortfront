# Development Backlog

## DOING (Current Work)

## TODO (Ordered by Priority)

### CST/AST Converter Enhancements (ENHANCEMENT)
- [ ] #483: feat: enable enhanced AST nodes with CST references
- [ ] #484: feat: implement full syntax construct conversion in CST to AST converter
- [ ] #485: test: add performance benchmarks for CST to AST conversion

### Code Quality and Refactoring (ENHANCEMENT)
- [ ] #365: refactor: break down large functions >200 lines (9 functions)
- [ ] #366: refactor: address remaining 24 functions exceeding 100-line limit
- [ ] #367: refactor: address remaining large files >1000 lines

### Type System Improvements (ENHANCEMENT)
- [ ] #401: Type System: Implement constraint generation for Hindley-Milner type inference
- [ ] #402: Type System: Implement unification algorithm with occurs check for constraint solving
- [ ] #403: Type System: Implement constraint solver with let-polymorphism support

### Testing and Documentation (SUPPORT)
- [ ] #474: parser: nested internal procedures not fully tracked in call graph analysis (DUPLICATE of #475)
- [ ] #475: parser: nested internal procedures not fully tracked in call graph analysis
- [ ] #470: tooling: FPM module dependency resolution bug blocking CST tests
- [ ] #467: fix: call graph test failures in main branch
- [ ] #468: fix: AST transformation test failures in main branch
- [ ] #464: test: call graph analysis tests failing due to known limitations
- [ ] #465: test: module parsing test failing for Issue #253
- [ ] #462: Tests run much too long on CI/CD
- [ ] #461: Get rid of -fmax-stack-var-size argument
- [ ] #456: Get rid of disabled tests. Either adapt to current system and enable, or delete if obsoleted
- [ ] #450: test: re-enable temporarily disabled tests for issues #4 and #321
- [ ] #451: test: complete AST arena integration test coverage
- [ ] #452: feature: restore debug output capabilities in frontend compilation pipeline

### Repository Maintenance (SUPPORT)
- [ ] #439: fix: rescue commits from main (repository hygiene - rescue branch cleanup)

### Documentation (SUPPORT) 
- [ ] #381: doc: create comprehensive arena architecture documentation

### Epic Planning and Architecture (FUTURE WORK)
- [ ] #392: epic: CST/AST Split Implementation - Complete Migration Plan
- [ ] #391: feat: CST Phase 5 - Implement advanced CST/AST features and external tool integration  
- [ ] #390: feat: CST Phase 4 - Remove legacy code and optimize CST/AST system
- [ ] #389: feat: CST Phase 3 - Migrate parser modules to CST-first construction
- [ ] #388: feat: CST Phase 2 - Implement CST to AST converter with bidirectional links
- [ ] #387: feat: CST Phase 1 - Implement parallel CST construction alongside existing AST
- [ ] #386: feat: CST Phase 0 - Create foundation infrastructure for CST/AST split
- [ ] #385: feat: complete Hindley-Milner type inference with constraint solving
- [ ] #384: feat: implement concrete syntax tree (CST) with trivia preservation
- [ ] #383: epic: unified arena architecture implementation roadmap
- [ ] #382: test: comprehensive arena testing suite
- [ ] #381: doc: create comprehensive arena architecture documentation
- [ ] #380: feat: create unified arena API for external tools (fluff, ffc)

## DONE (Completed)
- [x] #364: refactor: break down parse_declaration function (519 lines -> <100 lines) (COMPLETED - PR #477 merged with 89.3% size reduction, achieved 347→37 lines, fixed performance regression, optimized helper functions 198→96 lines (51% reduction), architectural excellence confirmed by Patrick)
- [x] #478: CRITICAL: PR #477 test failures/hangs blocking merge (COMPLETED - Performance regression successfully resolved by sergei, tests now complete in ~1 second vs >2 minutes, all functionality preserved)
- [x] #406: Refactor: Extract variable parsing and initialization logic from parse_declaration (COMPLETED - PR #476 merged with 74% function size reduction, extracted parse_variable_with_initialization helper (111 lines), streamlined parse_declaration to 37 lines)
- [x] #407: Refactor: Extract multi-variable declaration handling from parse_declaration (COMPLETED - PR #473 merged successfully with 59% size reduction, eliminated memory safety issues, and fixed critical parsing bugs affecting test_call_graph and test_module_parsing_bug_red)
- [x] #448: refactor: reduce function sizes in bench_arena_comparison.f90 (COMPLETED - Issue resolved as obsolete, referenced file does not exist in codebase)
- [x] #397: CST: Implement CST to AST converter with bidirectional linking (COMPLETED - PR #472 successfully implemented converter with memory corruption fixes, enabling CST/AST split roadmap completion)
- [x] #463: CRITICAL: System-wide regression from arena modernization (COMPLETED - sergei successfully resolved O(n²) performance bottleneck, eliminated memory corruption and segmentation faults, restored CI pipeline functionality)
- [x] #396: CST: Create CST builder for parallel construction alongside AST (COMPLETED - Implementation merged to main with fixed root handle bug, 12/12 tests passing, approved by Patrick)
- [x] #395: CST: Implement lexer trivia collection for comments and whitespace (COMPLETED - PR #469 successfully implemented lexer trivia collection system with comprehensive error handling and full test coverage)
- [x] #394: CST: Implement UID generation system for stable node identification (COMPLETED - PR #466 successfully implemented foundation UID system with 1012 tests passing and 0.008μs performance)
- [x] #441: Subroutines and functions not handled properly (COMPLETED - PR #460 successfully fixed parsing regression, core issue resolved)
- [x] #393: CST: Create basic CST node type definitions and module structure (COMPLETED - PR #455 implemented foundation CST types and arena management)
- [x] #454: Update tests to use modern AST arena API after Issue #360 (COMPLETED - PR #459 successfully modernized test suite with modern arena API)
- [x] #457: Parser fails on simple expressions with 'No statements found in file' (COMPLETED - PR #458 fixed arena size synchronization issue)
- [x] #360: Migrate AST to modern high-performance arena with unified architecture (COMPLETED - PR #453 merged with generation-based handles and unified architecture)
- [x] #371: feat: integrate compiler_arena for unified memory management (COMPLETED - Integrated into all compilation phases with comprehensive phase tracking)
- [x] #362: Create unified compiler_arena module for KISS architecture (COMPLETED - Module implemented with comprehensive tests)
- [x] #400: Arena: Create comprehensive performance benchmark suite for arena operations (branch: feat-arena-benchmark-suite-400 - 6 benchmark modules created)
- [x] #370: feat: migrate ast_arena to container API with type-bound procedures (PR #446 - Container API migration complete with full backward compatibility)
- [x] #369: feat: define base arena interface with type-bound procedures (COMPLETED - Base interface implemented for unified arena architecture)
- [x] #442: CRITICAL: Replace class(*) usage with abstract types for stable foundation (COMPLETED - Foundation ready for arena development)
- [x] #410: Error Handling: Migrate parser to use unified result_t with comprehensive error recovery (PR #434)
- [x] #399: Arena: Implement per-node freeing with generation tracking for selective memory management (PR #435)
- [x] #359: Implement arena memory allocator with generation-based safety (Comprehensive implementation complete)
- [x] #408: Error Handling: Design and implement unified result_t type for consistent error handling (PR #432)
- [x] #427: Error Handling: Migrate lexer to unified result_t error handling
