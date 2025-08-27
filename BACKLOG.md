# Development Backlog

## DOING (Current Work)

## SPRINT_BACKLOG - EMERGENCY RECOVERY SPRINT

**🚨 CRITICAL SYSTEM STATE**: Complete functional collapse - system outputs only TODO placeholders  
**Sprint Goal**: Restore basic functionality: working codegen, passing tests, compliant architecture  
**Definition of Done**: 
- Build produces working fortfront binary
- Codegen outputs actual Fortran code (not TODO placeholders)
- Test suite runs without crashes (no exit code 13)
- All files comply with <1000 line constraint
- Foundation issues resolved for future work

### PRIORITY 1: CRITICAL BLOCKERS - System Non-Functional

### EPIC: Complete Codegen System Failure (Issues #561, #563)
- [ ] #561: critical: duplicate generate_code_from_arena stubs causing circular dependency issues
- [ ] #563: bug: write statement codegen produces TODO placeholders instead of actual code

### PRIORITY 2: CRITICAL STABILITY - Test Suite Collapse  

### EPIC: Test Suite Crash Recovery (Issues #571, #575)
- [ ] #571: critical: massive test suite failure - 100+ tests failing with exit code 13
- [ ] #575: critical: sprint goal failure - fundamental system integrity compromised
- [ ] #577: architectural drift: error_stop usage violates library integration principles

### PRIORITY 3: CRITICAL ARCHITECTURE - Size Violations

### EPIC: Architectural Constraint Compliance (Issues #576, #556-560, #570)
- [ ] #576: architectural debt: 13 files exceed 1000-line architectural constraint
- [ ] #557: critical: file size violation in fortfront.f90 exceeds 1000 line limit
- [ ] #558: critical: file size violation in ast_factory.f90 exceeds 1000 line limit
- [ ] #559: critical: file size violation in parser_control_flow.f90 exceeds 1000 line limit
- [ ] #560: critical: file size violation in lexer_core.f90 exceeds 1000 line limit
- [ ] #570: critical: semantic_analyzer.f90 still exceeds 1000 line limit after Issue #532 refactoring

### PRIORITY 4: CRITICAL FUNCTIONALITY - Core Feature Failures

### EPIC: Codegen Functionality Restoration (Issues #572-574)
- [ ] #574: critical: complex multi-unit programs completely fail codegen - entire program structures lost
- [ ] #573: bug: subroutine with I/O statements generates empty output with TODO placeholders
- [ ] #572: bug: variable initialization and I/O statements generate only TODO placeholders in codegen

### EPIC: Parser & Semantic Core Functions (Issues #564-569)
- [ ] #564: bug: complex write statements with format specifiers fail to parse
- [ ] #565: bug: confusing error messages claim 'Parsing succeeded' when parsing fails
- [ ] #566: bug: multi-unit parsing generates incomplete output for module+program constructs
- [ ] #567: bug: malformed I/O statements parsed as valid but produce incorrect output
- [ ] #568: bug: no semantic error for duplicate variable declarations
- [ ] #569: bug: no semantic error for undeclared variables in I/O statements

### EPIC: Foundation Architecture Issues (Issues #546, #578, #550, #583, #584)
- [ ] #546: architectural drift: class(*) vtable linking issue #442 not resolved - blocking arena work
- [ ] #578: system integrity: foundation requirements vs implementation reality gap
- [ ] #550: design misalignment: FPM-first architecture not validated - external tool integration untested
- [ ] #583: refactor: resolve circular dependency between codegen_core and specialized modules
- [ ] #584: fix: implement proper code generation for complex nodes in codegen stub

### EPIC: Code Quality Issues (Issues #549, #562)
- [ ] #549: performance debt: test suite still has massive duplication - 230 tests despite consolidation claims
- [ ] #562: dead code: unused parameters in codegen placeholder functions

### EPIC: Sprint Documentation (Issue #581)
- [ ] #581: docs: consolidate emergency recovery sprint documentation

## POST-RECOVERY WORK (After Emergency Sprint)

### EPIC: Size Constraint Compliance (Deferred)
- [ ] #555: improve: clarify temp_type variable usage in semantic analyzer helpers
- [ ] #547: architectural violation: 12 files exceed 1000-line limit, violating size constraints  
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
