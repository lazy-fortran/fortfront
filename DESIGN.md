# fortfront Architecture Design - EMERGENCY RECOVERY

## 🚨 SPRINT 6: EMERGENCY SYSTEM RECOVERY - 2025-08-28

**CRISIS STATUS**: All build systems broken - development completely blocked  
**TECHNICAL EVIDENCE**: CMAKE fails (compiler_arena.mod), FPM fails (symbol_info_t API)  
**EMERGENCY GOAL**: Restore basic compilation capability before architectural work  
**VERIFIED DEFECTS**: 7 critical issues with technical evidence from PLAY audit

### BUILD SYSTEM CRISIS (Emergency State)

**CURRENT REALITY** (Verified 2025-08-28):
```bash
# CMAKE SYSTEM BROKEN:
mkdir -p cmake_build && cd cmake_build && cmake .. && make
# FAILS: Fatal Error: Cannot open module file 'compiler_arena.mod'

# FPM SYSTEM BROKEN:
fpm test --flag "-cpp -fmax-stack-var-size=524288"
# FAILS: Error: 'is_defined' not member of 'symbol_info_t' structure
```

**ROOT CAUSES IDENTIFIED**:
- CMAKE: Module dependency ordering broken in CMakeLists.txt
- FPM: symbol_info_t API inconsistency across modules
- BOTH: Development completely blocked, no working build path

### EMERGENCY SPRINT DEFINITION OF DONE

1. **BUILD SYSTEM BASIC FUNCTIONALITY**: Either CMAKE or FPM compiles successfully
2. **TYPE SYSTEM CONSISTENCY**: symbol_info_t API unified across all modules
3. **EXECUTABLE FUNCTIONALITY**: fortfront --version works without debug spam
4. **DOCUMENTATION ACCURACY**: CLAUDE.md reflects actual build system status

### CRITICAL DEFECTS TO ADDRESS (PLAY Audit Verified)

**SYSTEM-BREAKING COMPILATION FAILURES**:
1. **CMAKE Build**: Missing compiler_arena.mod dependency
2. **Symbol API**: Duplicate symbol_info_t definitions with incompatible fields
3. **FPM Tests**: 30+ compilation errors in symbol_management.f90
4. **UX Failure**: fortfront executable unusable (debug spam, broken --version)

**ARCHITECTURAL VIOLATIONS** (Deferred to Sprint 7):
1. **8 Files >1000 lines**: parser_import_statements.f90 (1302), variable_usage_tracker.f90 (1238), etc.
2. **Directory Organization**: src/ has 26 items (target: <15)
3. **Documentation Fraud**: CLAUDE.md contains systematically false claims

### EMERGENCY RECOVERY STRATEGY

**PHASE 1: Compilation System Recovery** (CRITICAL)
- Fix CMAKE module dependency ordering for compiler_arena.mod
- Resolve symbol_info_t duplicate definitions and API inconsistency
- Restore either CMAKE or FPM to basic compilation functionality
- Verify clean builds from scratch

**PHASE 2: Basic User Experience** (HIGH)
- Fix fortfront executable CLI interface (--version, help)
- Remove debug spam from normal operation
- Ensure basic Fortran transpilation functionality

**PHASE 3: Documentation Recovery** (MEDIUM)
- Update CLAUDE.md with accurate build system status
- Remove false claims about working systems
- Provide realistic assessment of current capabilities

**PHASE 4: Transition to Architecture Sprint** (FUTURE)
- Architectural violations deferred to Sprint 7
- Focus on systematic file size refactoring
- Address directory organization after build stability

### ARCHITECTURAL PRINCIPLES (Proven Effective)

**SIZE CONSTRAINTS** (Hard Limits):
- Files: <1000 lines (target <500 lines)
- Functions: <100 lines (target <50 lines)  
- Directories: <30 files (target <15 files)

**QUALITY HIERARCHY**:
CORRECTNESS > PERFORMANCE > KISS > SRP > YAGNI > DRY > SOLID > SECURITY

**REFACTORING STRATEGY** (Proven Successful):
1. **Identify Logical Groupings**: Related functions and types
2. **Extract Specialized Modules**: Clear single responsibilities
3. **Maintain Backward Compatibility**: Through re-exports if needed
4. **Incremental Validation**: Test after each extraction

### SUCCESS CRITERIA FOR EMERGENCY RECOVERY

**BASIC SYSTEM FUNCTIONALITY**:
- [ ] CMAKE build completes without module dependency errors
- [ ] FPM test execution completes without symbol_info_t API errors
- [ ] fortfront --version displays version information (not debug output)
- [ ] CLAUDE.md accurately describes current build system status

**DEVELOPMENT UNBLOCKED**:
- [ ] New contributors can follow working build instructions
- [ ] Basic compilation path restored for ongoing development
- [ ] Test execution possible for code verification
- [ ] Foundation ready for architectural recovery in Sprint 7

---

## Emergency Recovery Approach

**PRINCIPLE**: Restore basic functionality first, architectural integrity second  
**APPROACH**: Fix critical compilation blockers before attempting systematic refactoring  
**OUTCOME**: Working build system enables safe architectural improvements in Sprint 7

*This emergency recovery addresses critical system failures identified through fraud-proof PLAY phase technical validation. Architectural violations deferred until basic development capabilities restored.*