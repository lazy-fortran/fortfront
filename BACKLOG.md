# Development Backlog

## DOING (Current Work)

### CRITICAL SPRINT ASSESSMENT REQUIRED
**EPIC: System State Analysis** 
- [ ] #634: architectural: foundation-first development protocol implementation required
- [ ] #633: architectural: systemic development process improvements required  
- [ ] #632: CRITICAL: SHORT sprint success assessment reveals mixed system state
- [ ] #631: CRITICAL: SHORT sprint structural audit reveals systematic development process failure

## SPRINT_BACKLOG - CONTROL FLOW RESTORATION

**Sprint Goal**: Expand working functionality to include control flow constructs  
**Definition of Done**: 
- Basic control flow works: if/then/else, do loops generate valid Fortran (not TODO placeholders)
- Function definitions and calls generate valid Fortran code
- Multi-variable declarations work correctly  
- All generated code compiles without syntax errors
- System supports programs with mixed basic + control flow constructs

**Sprint Strategy**: Progressive expansion - build on SHORT sprint's 30% success  
**Foundation**: Keep working basic functionality (print, assignments) intact

### EPIC: Control Flow Code Generation
- [ ] #623: bug: if/else statements generate TODO placeholders instead of code
- [ ] #620: bug: do loop statements generate TODO placeholders instead of code

### EPIC: Function Support Restoration  
- [ ] #622: bug: function definitions generate TODO placeholders instead of code

### EPIC: Variable Declaration Fixes
- [ ] #618: bug: multi-variable declarations corrupted in code generation
- [ ] #621: bug: array declarations completely corrupted in code generation

### EPIC: Critical Architecture Violations  
- [ ] #630: architectural violation: 12 files still exceed 1000-line limit after claimed resolution
- [ ] #629: architectural defect: TODO placeholders throughout entire codegen system
- [ ] #607: bug: architectural violation - error_stop usage still present in 17 files
- [ ] #605: bug: critical size constraint violations - 9 files exceed 1000 line architectural limit

### EPIC: Code Quality Fixes
- [ ] #619: bug: debug output contaminating generated code output

### EPIC: Test Suite Critical Issues
- [ ] #628: CRITICAL: SHORT sprint false success claims - basic functionality completely broken
- [ ] #627: CRITICAL: test suite false positives mask major functionality failures  
- [ ] #625: bug: test suite false positives - tests pass despite generating TODO placeholders
- [ ] #624: CRITICAL: SHORT sprint claims false - major functionality completely broken
- [ ] #604: bug: test suite hangs indefinitely during full execution

### EPIC: Critical Code Generation Failures
- [ ] #626: CRITICAL: core codegen body generation returns TODO placeholders
- [ ] #606: bug: critical TODO placeholders still present throughout codegen system
- [ ] #603: bug: code generation failure - module variable declarations missing  
- [ ] #602: bug: critical code generation failure - subroutines completely mangled and empty
- [ ] #601: bug: critical code generation failure - all statements missing from program body
- [ ] #609: bug: character string example failure - incomplete type declarations and missing assignments

### EPIC: System Crisis Issues
- [ ] #612: system integrity failure: emergency recovery sprint achieved 0% of goals - complete development halt required
- [ ] #611: architectural crisis: foundation architecture completely unstable - requires emergency intervention
- [ ] #610: critical: emergency recovery sprint FAILED - system remains completely non-functional

### EPIC: Sprint Documentation
- [ ] #613: docs: consolidate control flow restoration sprint fixes

## DEFERRED TO FUTURE SPRINTS

### HIGH PRIORITY - Core Parser Gaps
- [ ] #492: Statement parsing: Semicolon-separated statements only process first statement (parser completeness)
- [ ] #495: Semantic analysis: Undefined variables not detected in expressions (type system gap)
- [ ] #493: Operator precedence: Incorrect logical operator precedence and parenthesization (correctness)

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
