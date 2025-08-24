# Development Backlog

## DOING (Current Work)
- [ ] CRITICAL SYSTEM-WIDE REGRESSION: Arena modernization PRs (#455, #459) cause infinite timeout loops across entire test suite (HANDOFF TO SERGEI)

**🚨 URGENT: SYSTEM-WIDE CRITICAL FAILURE**
- **Scope**: Affects main branch AND all feature branches (not just PR #460)
- **Symptoms**: Tests timeout with infinite "STOP 0" loops, segmentation faults, CI failures
- **Root Cause**: Arena modernization work from PRs #455, #459 introduced architectural instability
- **Impact**: BLOCKING all development - entire test suite non-functional
- **Action**: IMMEDIATE SERGEI ATTENTION required for system architecture fix

## TODO (Ordered by Priority)

### CRITICAL: Foundation Architecture (BLOCKING ALL OTHER WORK)
No remaining items - Foundation architecture complete

### Foundation Phase 1: Error Handling Infrastructure (CRITICAL)
No remaining items - Phase 1 complete

### Foundation Phase 2: Arena System (Foundation complete, continuing development)
No remaining items - Phase 2 development continuing

### Foundation Phase 3: CST/AST Infrastructure (BLOCKED until #442 complete)
- [ ] #394: CST: Implement UID generation system for stable node identification  
- [ ] #395: CST: Implement lexer trivia collection for comments and whitespace
- [ ] #396: CST: Create CST builder for parallel construction alongside AST
- [ ] #397: CST: Implement CST to AST converter with bidirectional linking

### Code Quality and Refactoring (ENHANCEMENT)
- [ ] #448: refactor: reduce function sizes in bench_arena_comparison.f90
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

### Repository Maintenance (SUPPORT)
- [ ] #439: fix: rescue commits from main (repository hygiene - rescue branch cleanup)

### Testing and Documentation (SUPPORT)
- [ ] #456: Get rid of disabled tests. Either adapt to current system and enable, or delete if obsoleted
- [ ] #450: test: re-enable temporarily disabled tests for issues #4 and #321
- [ ] #451: test: complete AST arena integration test coverage
- [ ] #452: feature: restore debug output capabilities in frontend compilation pipeline
- [ ] #361: Create GCC Bug 114612 test suite and permanent regression prevention

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
- [x] #393: CST: Create basic CST node type definitions and module structure (COMPLETED - PR #455 implemented foundation CST types and arena management)
- [x] #454: Update tests to use modern AST arena API after Issue #360 (COMPLETED - PR #459 successfully modernized test suite with modern arena API)
- [x] #441: Subroutines and functions not handled properly (COMPLETED - PR #460 fixed critical parsing regression with stable solution)
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
