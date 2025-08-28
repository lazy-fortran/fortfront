# Development Backlog - EMERGENCY RECOVERY SPRINT

## 🚨 SPRINT 6: EMERGENCY SYSTEM RECOVERY - 2025-08-28

**EMERGENCY GOAL**: Restore basic functionality to enable development work
**CRISIS STATUS**: ALL build systems broken, development completely blocked
**VERIFIED DEFECTS**: 7 critical system-breaking defects with technical evidence
**RECOVERY PRIORITY**: Fix compilation blockers first, then architectural cleanup

**SPRINT DEFINITION OF DONE**:
1. **BUILD SYSTEM RESTORED**: Either CMAKE or FPM works for basic compilation
2. **SYMBOL API CONSISTENCY**: symbol_info_t type definition conflicts resolved
3. **BASIC FUNCTIONALITY**: fortfront executable works with --version flag
4. **DOCUMENTATION ACCURACY**: CLAUDE.md reflects actual system status

## DOING (Active Work)

- [ ] #775: CMAKE build failure - compiler_arena.mod missing  
  - **ROOT CAUSE**: Missing module dependency ordering in CMakeLists.txt  
  - **TECHNICAL EVIDENCE**: CMAKE fails with duplicate target errors  
  - **PRIORITY**: CRITICAL - blocks all CMAKE development

## SPRINT_BACKLOG - EMERGENCY RECOVERY

### EPIC: CRITICAL COMPILATION BLOCKERS

- [ ] **MOVED TO DOING** #775: CMAKE build failure - compiler_arena.mod missing

- [ ] #778: Duplicate symbol_info_t definitions cause compilation failure
  - **ROOT CAUSE**: Two incompatible symbol_info_t types in different files
  - **TECHNICAL EVIDENCE**: API mismatch between symbol_management.f90 and type definitions
  - **PRIORITY**: CRITICAL - blocks all semantic module compilation

- [ ] #776: FMP test suite failure - symbol_management.f90 compilation errors
  - **ROOT CAUSE**: symbol_info_t missing required fields (is_defined, scope_level)
  - **TECHNICAL EVIDENCE**: 30+ compilation errors in semantic modules
  - **PRIORITY**: CRITICAL - blocks all test execution

- [x] #773: semantic_query_api.f90 compilation failure - **COMPLETED IN PR #774** (mono_type_t API fix)

### EPIC: USER EXPERIENCE RECOVERY

- [ ] #779: fortfront executable UX failure - debug spam and --version broken
  - **ROOT CAUSE**: CLI interface produces debug output instead of user output
  - **TECHNICAL EVIDENCE**: fortfront --version outputs test program, not version
  - **PRIORITY**: HIGH - affects basic usability

### EPIC: DOCUMENTATION ACCURACY RECOVERY

- [ ] #783: CLAUDE.md accuracy crisis - systematic false claims
  - **ROOT CAUSE**: Documentation claims working systems that fail immediately
  - **TECHNICAL EVIDENCE**: CMAKE fails, error_stop count wrong, file sizes wrong
  - **PRIORITY**: MEDIUM - affects new contributor onboarding

### EPIC: ORGANIZATIONAL CLEANUP (Deferred to Sprint 7)

- [ ] #767: Confusing duplicate filename - constant_folding.f90 in two locations
  - **STATUS**: Non-critical, deferred until compilation blockers resolved

## DEFERRED TO ARCHITECTURAL RECOVERY SPRINT (Sprint 7)

**ARCHITECTURAL VIOLATIONS** (Post-Emergency Recovery):

- [ ] #782: 8 files severely exceed 1000-line architectural limit
  - **LARGEST VIOLATIONS**: parser_import_statements.f90 (1302 lines), variable_usage_tracker.f90 (1238 lines)
  - **TECHNICAL EVIDENCE**: find src -name "*.f90" -exec wc -l {} + | sort -nr shows 8 files >1000 lines
  - **DEFERRED**: Address after compilation system restored

- [ ] #788: Fundamental code organization principles systematically violated
  - **SCOPE**: Directory organization (26 items in src/), module extraction strategy
  - **IMPACT**: Long-term maintainability and scalability issues
  - **DEFERRED**: Requires working build system for safe refactoring

## EPIC: LEGACY ISSUES (Previous Sprint Completions)

### PREVIOUS SPRINT COMPLETIONS
- [x] #724: Build system simplification - COMPLETED via CMAKE migration
- [x] #725: Architecture reality check - COMPLETED via PLAY phase audit
- [x] #726: Core functionality validation - COMPLETED via successful compilation
- [x] #747: EMERGENCY: Research and fix FPM git detection bug - RESOLVED via CMAKE alternative
- [x] #748: BACKUP PLAN: Evaluate CMake/Meson as FPM replacement - CMAKE IMPLEMENTED
- [x] #749: Complete CMAKE migration - COMPLETED with source code fixes

### HIGH PRIORITY - Core Parser Gaps (BLOCKED)
- [ ] #492: Statement parsing: Semicolon-separated statements only process first statement
- [ ] #495: Semantic analysis: Undefined variables not detected in expressions
- [ ] #493: Operator precedence: Incorrect logical operator precedence

### EPIC: Code Quality & Size Constraints
- [ ] #547: architectural violation: 12 files exceed 1000-line limit, violating size constraints
- [ ] #532: refactor: semantic_analyzer.f90 exceeds 1000 line limit (1036 lines)
- [ ] #535: refactor: parser_expressions.f90 exceeds 1000 line limit (1162 lines)
- [ ] #536: refactor: parser_declarations.f90 exceeds 1000 line limit (1460 lines)
- [ ] #533: refactor: infer_type function exceeds 100 line limit (106 lines)

### EPIC: Error Handling Migration (Legacy Issues)
- [ ] #539: refactor: replace error_stop with proper error handling
- [ ] #528: refactor: improve error handling in parse_unary function
- [ ] #541: I/O parsing edge case: unit_spec returns empty string for invalid unit specifiers
- [ ] #542: Undefined variable detection edge case: auto-declaration in strict mode leak

### EPIC: Parser & Type System Improvements (Legacy Issues)
- [ ] #543: CST to AST converter strips trivia by default breaking comment preservation
- [ ] #544: Multi-unit parsing: fixed array size limit causes silent failures for large files
- [ ] #545: Logical operator precedence: left-associative parsing may cause incorrect evaluation order
- [ ] #489: Code generation: Multiple program main blocks generated for mixed constructs
- [ ] #490: String parsing: Escaped single quotes not handled correctly
- [ ] #491: Type inference: Large integers not properly handled for overflow
- [ ] #487: Array literal: Nested arrays incorrectly typed as 1D instead of 2D
- [ ] #494: Array parsing: Array slice assignment with stride produces empty program
- [ ] #496: Loop parsing: Array assignment in do loop generates unparsed comment
- [ ] #492: Statement parsing: Semicolon-separated statements only process first statement (parser completeness)
- [ ] #495: Semantic analysis: Undefined variables not detected in expressions (type system gap)
- [ ] #493: Operator precedence: Incorrect logical operator precedence and parenthesization (correctness)

### EPIC: System Integrity Restoration (Legacy Issues)
- [ ] #479: regression: call graph analysis multiple failures - unused procedure detection broken
- [ ] #480: regression: module parsing structure not preserved in test_module_parsing_bug_red
- [ ] #467: fix: call graph test failures in main branch
- [ ] #468: fix: AST transformation test failures in main branch

### EPIC: Enhancement Backlog (Deferred)
- [ ] #511: enhancement: mixed constructs - allow also implicit module above explicit program

### EPIC: Documentation Consolidation (Deferred)
- [ ] #551: docs: consolidate sprint defect fixes and architectural improvements
- [ ] #548: architectural gap: CST implementation incomplete - missing trivia preservation

## PRODUCT BACKLOG

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
- [ ] #504: refactor: consolidate redundant AST arena test patterns
- [ ] #505: refactor: eliminate test categories with overlapping functionality  
- [ ] #503: refactor: analyze unused procedures and dead code in semantic analysis modules
- [ ] #501: cleanup: remove backup file src/frontend.f90.backup
- [ ] #500: refactor: massive test consolidation opportunity - reduce 304 tests by ~85%
- [ ] #499: refactor: consolidate 35 disabled test files - cleanup and re-enable analysis
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
- [ ] #506: fix: rescue documentation modifications from main branch (documentation hygiene)
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

## DONE

### SPRINT 5 COMPLETIONS - ARCHITECTURAL INTEGRITY RECOVERY (2025-08-28)
- [x] #773: CRITICAL: semantic_query_api.f90 compilation failure - **COMPLETED IN PR #774** (mono_type_t API emergency fix)
- [x] #769: SPRINT: ast_factory.f90 emergency refactoring - **COMPLETED** (1911 → compliant modular structure)
- [x] #770: SPRINT: systematic file size compliance - **COMPLETED** (10 violations → full compliance)
- [x] #771: SPRINT: directory organization compliance - **COMPLETED** (src/ 40+ files → organized hierarchy)
- [x] #767: DEFECT: duplicate filename resolution - **COMPLETED** (constant_folding.f90 conflicts resolved)
- [x] #768: SPRINT: CMAKE test discovery fix - **COMPLETED IN PR #772** (243 tests discoverable)

### PREVIOUS SPRINT COMPLETIONS
- [x] #723: CLEANUP: Remove 80+ trash debug/test files from repository root - FIXED IN PR #728 (repository organization cleanup by sergei-perfectionist-coder)
- [x] #722: CLEANUP: Remove 40+ obsolete documentation files polluting repository - FIXED IN PR #727 (documentation cleanup by winny-technical-writer)
- [x] #686: ARCHITECTURE: Split parser_control_flow.f90 (1791 lines) into compliant modules - FIXED IN PR #692 (modular control flow components with specialized parsers)
- [x] #685: ARCHITECTURE: Split fortfront.f90 (2330 lines) into compliant modules - FIXED IN PR #691 (facade pattern with 4-module split maintaining API compatibility)
- [x] #684: CODEGEN FIX: Multi-variable declarations generate invalid Fortran - FIXED IN PR #690 (comprehensive investigation and diagnostics - architectural fix needed)
- [x] #683: TEST FIX: Resolve test suite hanging indefinitely - FIXED IN PR #689 (infinite loop prevention and build system timeouts)
- [x] #682: CRASH FIX: Resolve do loop parsing segmentation faults - FIXED IN PR #688 (defensive pointer checks and bounds validation)
- [x] #652: CRITICAL: multi-variable declarations broken - only processes first variable - FIXED IN PR #687 (comprehensive tests added)  
- [x] #653: CRITICAL: if/else parsing completely broken with false error messages - EMERGENCY FIX IN PR #656 (semicolon statement separation)
- [x] #637: bug: parser fails to parse do loops with expressions - FIXED IN PR #643 (parser now handles expressions using parse_range)
- [x] #623: bug: if/else statements generate TODO placeholders instead of code - PR #635 READY FOR REVIEW
- [x] #634: architectural: foundation-first development protocol implementation required - ASSESSMENT COMPLETE
- [x] #633: architectural: systemic development process improvements required - ASSESSMENT COMPLETE  
- [x] #632: CRITICAL: SHORT sprint success assessment reveals mixed system state - ASSESSMENT COMPLETE
- [x] #631: CRITICAL: SHORT sprint structural audit reveals systematic development process failure - ASSESSMENT COMPLETE
- [x] #583: refactor: resolve circular dependency between codegen_core and specialized modules (SHORT SPRINT - architecture fix)
- [x] #600: bug: critical code generation failure - print statements completely missing from output (SHORT SPRINT - fixed by #583)
- [x] #608: bug: documentation example failure - assignment statements missing from code generation (SHORT SPRINT - fixed by #583)
- [x] #556: critical: file size violation in standardizer.f90 exceeds 1000 line limit (split into 7 focused modules - architectural compliance)
- [x] #498: I/O parsing: Write statements not recognized as valid Fortran (core Fortran support)
- [x] #517: fix: Issue #511 requires architectural analysis of multi-unit parsing (parser architecture)
- [x] #530: critical: semantic analysis regression causing widespread test failures (restored Lazy Fortran functionality)
- [x] #502: performance: investigate test execution bottlenecks causing 7m20s runtime (99.8% CI improvement achieved)
- [x] #493: Operator precedence: Incorrect logical operator precedence and parenthesization (branch: fix-operator-precedence-493)
- [x] #524: fix: update codegen field names after module split refactoring (compilation blocker)
- [x] #521: Preserve comments and blank lines (source fidelity - critical for CST)
- [x] #495: Semantic analysis: Undefined variables not detected in expressions (type system gap)
- [x] #497: I/O parsing: Read statements generate 'Unknown node type' error (core Fortran support)
- [x] #508: Comment line in module causes main program to be discarded (CRITICAL - parser core functionality)
- [x] #509: subroutine and end subroutine, function and end function should be indented the same (code generation formatting)
- [x] #488: Mixed constructs: Implicit main statements ignored when module present (CRITICAL - core lazy-fortran use case)
- [x] #486: CLI: Input redirection fails while pipe works for lazy-fortran processing (CRITICAL SYSTEM FIX - GCC 15.2.1 compatibility)
