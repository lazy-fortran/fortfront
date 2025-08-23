# fortfront Development Roadmap

**FPM-FIRST ARCHITECTURE**
- **Dependency Management**: FPM automatically handles all tool dependencies via fpm.toml
- **Pure Fortran**: External tool integration through standard Fortran module interfaces
- **Simple Integration**: Tools add `fortfront` as dependency - FPM handles compilation and linking
- **No Build Complexity**: FPM manages all module compilation and linking automatically

This roadmap focuses on core functionality rather than unnecessary build complexity.

## 🔥 **PHASE 0: FPM INTEGRATION VERIFICATION** (ABSOLUTE PRIORITY)

**PRIORITY**: Verify FPM handles all dependency management correctly

### Step 1: FPM Dependency Verification
- **Test Integration**: Create sample external tool using fortfront as FPM dependency  
- **Verify Compilation**: Confirm FPM correctly compiles and links all fortfront modules
- **Test Functionality**: Ensure external tools can access fortfront functionality
- **Document Patterns**: Create simple integration examples

### Step 2: API Surface Review
- **Module Identification**: Identify which existing modules should be public API
- **Interface Cleanup**: Ensure clean module interfaces for external tool use  
- **Documentation**: Document public API patterns and usage
- **Integration Guide**: Create getting-started guide for external tool developers

### Step 3: Cross-Platform Testing
- **Platform Verification**: Test FPM integration across platforms
- **Compiler Testing**: Verify with different Fortran compilers
- **Tool Examples**: Create examples for different tool types (linter, compiler, formatter)
- **Performance**: Verify no significant overhead from FPM dependency management

**DELIVERABLE**: Verified FPM integration with documented patterns for external tool development

## 🏗️ **PHASE 1: Core Arena Foundation** (After Phase 0 Complete)

### Arena System (Foundation Performance)
1. **#359** - Implement arena memory allocator with generation-based safety
2. **#369** - Define base arena interface with type-bound procedures  
3. **#370** - Migrate ast_arena to container API with type-bound procedures
4. **#398** - Arena: Implement arena handle validation with generation checking
5. **#399** - Arena: Implement per-node freeing with generation tracking

### Phase 2: Critical Bug Fixes
6. **#329** - Fix character type inference for string literals and expressions
7. **#354** - GCC 15.2.1 compatibility issue with allocatable components

### Phase 3: Error Handling Foundation  
8. **#408** - Error Handling: Design and implement unified result_t type
9. **#409** - Error Handling: Migrate lexer to use unified result_t
10. **#410** - Error Handling: Migrate parser to use unified result_t

## 🏗️ **HIGH PRIORITY** (CST/AST Split Foundation)

### Phase 4: CST Infrastructure
11. **#393** - CST: Create basic CST node type definitions and module structure
12. **#394** - CST: Implement UID generation system for stable node identification  
13. **#395** - CST: Implement lexer trivia collection for comments and whitespace
14. **#396** - CST: Create CST builder for parallel construction alongside AST
15. **#397** - CST: Implement CST to AST converter with bidirectional linking

## 📈 **MEDIUM PRIORITY** (Code Quality & Architecture)

### Phase 5: Function Refactoring (Enables Better Testing)
16. **#404** - Extract type specifier parsing from parse_declaration
17. **#405** - Extract attribute parsing logic from parse_declaration  
18. **#406** - Extract variable parsing and initialization from parse_declaration
19. **#407** - Extract multi-variable declaration handling from parse_declaration
20. **#365** - Refactor: break down large functions >200 lines (9 functions)
21. **#366** - Address remaining 24 functions exceeding 100-line limit

### Phase 6: Extended Arena System
22. **#372** - Refactor: eliminate duplication between arena implementations
23. **#373** - Feat: implement type arena with container API
24. **#377** - Feat: create symbol arena for scope management
25. **#378** - Feat: create literal arena for string/constant pooling

## 🚀 **ADVANCED FEATURES** (Type System & Performance)

### Phase 7: Type System Enhancement
26. **#401** - Type System: Implement constraint generation for HM inference
27. **#402** - Type System: Implement unification algorithm with occurs check
28. **#403** - Type System: Implement constraint solver with let-polymorphism

### Phase 8: Performance & Testing
29. **#400** - Arena: Create comprehensive performance benchmark suite
30. **#376** - Perf: add arena performance benchmarks and monitoring
31. **#382** - Test: comprehensive arena testing suite

## 🔄 **COMPLETION PHASES** (Integration & Polish)

### Phase 9: CST/AST Migration Completion
32. **#386** - CST Phase 0: Foundation infrastructure *(may be completed by earlier work)*
33. **#387** - CST Phase 1: Parallel CST construction *(may be completed by earlier work)*
34. **#388** - CST Phase 2: CST to AST converter *(may be completed by earlier work)*
35. **#389** - CST Phase 3: Migrate parser modules to CST-first
36. **#390** - CST Phase 4: Remove legacy code and optimize system
37. **#391** - CST Phase 5: Advanced features and external tool integration

### Phase 10: Enhanced Capabilities
38. **#379** - Add arena checkpoint and rollback for speculative operations
39. **#374** - Add validated handle access with safety checks
40. **#380** - Create unified arena API for external tools

## 📚 **DOCUMENTATION & LONG-TERM**

### Phase 11: Documentation & Enhancement
41. **#367** - Address remaining large files >1000 lines
42. **#381** - Create comprehensive arena architecture documentation
43. **#340** - Introduce standard dynamic container utilities
44. **#339** - Refactor module import patterns

### Phase 12: Strategic Features (After Core Stable)
45. **#361** - Create GCC Bug 114612 test suite
46. **#345** - Bootstrap: Preprocessor for generics subset  
47. **#344** - Meta: Roadmap for Fortran 95 support

## **EPIC TRACKING** (Don't Process Directly)
- **#392** - Epic: CST/AST Split Implementation  
- **#383** - Epic: Unified arena architecture roadmap

---

## 🎯 **GETTING STARTED**

**START HERE**: Issues #359, #369, #370 form the critical foundation that everything else builds on.

## **PROCESSING PRINCIPLES**

This roadmap ensures:

✅ **System Integrity**: System remains functional after every merge  
✅ **Dependency Management**: Dependencies are handled in correct order  
✅ **Solid Foundation**: Core infrastructure before advanced features  
✅ **Performance First**: Performance improvements come early in the pipeline  
✅ **No Legacy Code**: Complete cleanup with no redundancies or commented sections  
✅ **Incremental Progress**: Each issue can be completed independently  

## **PHASE DEPENDENCIES**

- **Phase 1** must complete before Phase 4 (CST needs arena foundation)
- **Phase 2** can run in parallel with Phase 1
- **Phase 3** can run in parallel with Phase 1-2  
- **Phase 4** depends on Phase 1 completion
- **Phases 5-6** can run in parallel after Phase 3
- **Phase 7** depends on Phase 3 completion
- **Phase 8** can run in parallel with Phase 7
- **Phase 9** depends on Phase 4 completion
- **Phases 10-12** depend on Phase 9 completion

## **SUCCESS METRICS**

Each phase completion should achieve:
- All tests passing
- No performance regressions
- Complete documentation for new features
- Zero legacy code remaining
- Full integration with existing system