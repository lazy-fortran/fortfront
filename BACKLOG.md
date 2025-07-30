# FORTFRONT DEVELOPMENT BACKLOG

## 🎯 MISSION
Transform fortfront from a basic lazy Fortran parser into a robust, modular, and feature-complete Fortran95 frontend with excellent test coverage.

## 📊 CURRENT STATUS
- **Total test files**: 122
- **Tests with known limitations**: 11
- **AST core module**: 3907 lines (GIGANTIC - needs splitting)
- **Major limitation**: Functions, scoping, and complex language constructs

---

## 🏗️ PHASE I: ARCHITECTURAL REFACTORING

### 1.1 Split the Gigantic `ast_core.f90` (3907 lines)

**Priority**: 🔴 CRITICAL  
**Estimated effort**: 2-3 days

Current `ast_core.f90` contains:
- Base AST infrastructure (ast_node, ast_entry_t, ast_arena_t)
- 40+ AST node types (program_node, assignment_node, function_def_node, etc.)
- Arena management logic
- Visitor pattern interfaces

**Split into:**
```
src/ast/
├── ast_base.f90          # Base ast_node, visitor interfaces, core types
├── ast_arena.f90         # Arena management (ast_arena_t, ast_entry_t)  
├── ast_nodes_core.f90    # Essential nodes (program, assignment, identifier, literal)
├── ast_nodes_control.f90 # Control flow (if, do, select, where, forall)
├── ast_nodes_procedure.f90 # Functions, subroutines, calls
├── ast_nodes_data.f90    # Declarations, derived types, interfaces
├── ast_nodes_io.f90      # I/O statements (print, write, read, format)
└── ast_factory.f90       # Node creation helpers (already exists)
```

**Acceptance criteria:**
- [ ] All existing tests still pass
- [ ] Each module < 800 lines
- [ ] Clear module dependencies
- [ ] No circular dependencies
- [ ] Proper public/private interfaces

### 1.2 Improve Build System & Testing
- [ ] Add module dependency visualization
- [ ] Implement test categorization (unit, integration, e2e)
- [ ] Add test coverage reporting
- [ ] Create CI/CD pipeline configuration

---

## 🚀 PHASE II: PARSER ENHANCEMENT

### 2.1 Function Definition & Scoping

**Priority**: 🔴 HIGH  
**Current limitation**: Functions generate basic programs instead of proper function definitions

**Target failing tests:**
- `test_type_inference_edge_cases` Test 1 (Recursive functions)
- `test_type_inference_edge_cases` Test 2 (Variable shadowing/scoping)
- Function-related API tests

**Tasks:**
- [ ] Implement proper function/subroutine parsing in `parser_declarations.f90`
- [ ] Add scope management to semantic analyzer
- [ ] Support local variable declarations within functions
- [ ] Handle function return types and return statements
- [ ] Implement recursive function detection

**Test targets:**
```fortran
! Should work after this phase:
real function factorial(n)
    integer, intent(in) :: n
    if (n <= 1) then
        factorial = 1.0
    else
        factorial = n * factorial(n-1)
    end if
end function factorial
```

### 2.2 Control Flow Enhancements

**Priority**: 🟡 MEDIUM

**Missing constructs:**
- [ ] `forall` constructs (test_parser_control_flow_direct.f90:28)
- [ ] Complex `where` constructs  
- [ ] `select case` with ranges
- [ ] Nested control structures

### 2.3 Expression Parser Improvements

**Priority**: 🟡 MEDIUM

**Missing operators:**
- [ ] `.eqv.` and `.neqv.` logical operators
- [ ] Complex number arithmetic
- [ ] Array section syntax (`arr(1:5:2)`)
- [ ] Implied DO loops in array constructors

---

## 🧮 PHASE III: SEMANTIC ANALYSIS COMPLETION

### 3.1 Advanced Type Inference

**Priority**: 🟡 MEDIUM

**Current gaps:**
- [ ] Polymorphic function constraints (Test 5 in edge cases)
- [ ] Generic interfaces
- [ ] Derived type member access
- [ ] Array conformance checking

### 3.2 Scope & Symbol Management

**Priority**: 🔴 HIGH

**Missing features:**
- [ ] Proper variable scoping in functions/subroutines
- [ ] Module variable visibility
- [ ] `use` statement handling with `only` clauses
- [ ] Symbol name resolution across scopes
- [ ] Undeclared variable detection

**Target failing tests:**
- `test_undeclared_variable_error.f90`
- `test_intent_semantic_check.f90` (INTENT violation detection)

### 3.3 Intent & Attribute Checking

**Priority**: 🟡 MEDIUM

- [ ] INTENT(IN) modification detection
- [ ] PARAMETER constant usage validation
- [ ] ALLOCATABLE status tracking
- [ ] POINTER association validation

---

## 🏭 PHASE IV: CODE GENERATION IMPROVEMENTS

### 4.1 Advanced Array Features

**Priority**: 🟡 MEDIUM

**Missing features:**
- [ ] Implied DO loop code generation
- [ ] Array constructor optimization
- [ ] Multi-dimensional array slicing
- [ ] Array reduction operations

**Target failing tests:**
- `test_codegen_green.f90` (Implied do codegen)
- `test_array_features_pipeline.f90` (Array constructors)

### 4.2 I/O Statement Generation

**Priority**: 🟢 LOW

- [ ] Formatted I/O with format descriptors
- [ ] Namelist I/O
- [ ] File positioning statements
- [ ] Advanced format specifications

### 4.3 Memory Management

**Priority**: 🟡 MEDIUM

**Missing features:**
- [ ] `allocate`/`deallocate` statement parsing and generation
- [ ] Automatic deallocation
- [ ] Memory leak detection (optional)

**Target failing tests:**
- `test_allocate_deallocate_parsing.f90`

---

## 🧪 PHASE V: COMPREHENSIVE FORTRAN95 SUPPORT

### 5.1 Advanced Language Features

**Priority**: 🟢 LOW (Future expansion)

- [ ] Generic interfaces and operator overloading
- [ ] Parameterized derived types (Fortran 2003 preview)
- [ ] Abstract interfaces
- [ ] Procedure pointers (basic support)

### 5.2 Module System Enhancement

**Priority**: 🟡 MEDIUM

- [ ] Complex module dependencies
- [ ] Module procedures
- [ ] Module variables with initialization
- [ ] Submodules (Fortran 2008 preview)

### 5.3 Interoperability Features

**Priority**: 🟢 LOW

- [ ] C interoperability (ISO_C_BINDING)
- [ ] External procedure interfaces
- [ ] Mixed-language programming support

---

## 📋 TESTING STRATEGY

### Existing Test Categories
- **Lexer tests**: 8 files (robust coverage)
- **Parser tests**: 22 files (needs expansion)
- **Semantic tests**: 15 files (needs major work) 
- **Codegen tests**: 18+ files (good coverage)
- **API tests**: 12 files (comprehensive)
- **Integration tests**: 5+ files (solid coverage)

### New Test Categories Needed
- [ ] **Scoping tests**: Variable visibility, function locals
- [ ] **Error handling tests**: Better error messages and recovery
- [ ] **Performance tests**: Large file parsing, memory usage
- [ ] **Compliance tests**: Fortran95 standard conformance
- [ ] **Regression tests**: Prevent feature breakage

### Test-Driven Development Process
1. **RED**: Write failing test for new feature
2. **GREEN**: Implement minimal code to pass test
3. **REFACTOR**: Clean up implementation
4. **INTEGRATE**: Ensure no existing tests break

---

## 🎯 SUCCESS METRICS

### Phase I Completion (Architecture)
- [ ] `ast_core.f90` split into 6-8 focused modules
- [ ] All 122 existing tests still pass
- [ ] Build time improved by >20%
- [ ] Module dependency graph visualized

### Phase II Completion (Parser)
- [ ] Function definitions properly parsed and generated
- [ ] Variable scoping works across function boundaries  
- [ ] Recursive functions detected and marked
- [ ] 0/6 "EXPECTED FAILURE" tests in edge cases (all real passes)

### Phase III Completion (Semantics)
- [ ] Undeclared variable detection working
- [ ] INTENT violation detection implemented
- [ ] Advanced type inference for 80% of Fortran95 constructs
- [ ] Memory-safe semantic analysis (no segfaults)

### Phase IV Completion (Codegen)
- [ ] All major Fortran95 constructs generate correct code
- [ ] Implied DO loops, array constructors working
- [ ] Generated code compiles with gfortran -std=f95
- [ ] Code generation performance benchmarked

### Final Success Criteria
- [ ] **Test coverage**: >95% of Fortran95 constructs
- [ ] **Performance**: Parse 10K line files in <2 seconds
- [ ] **Robustness**: Handle malformed input gracefully
- [ ] **Standards compliance**: Generate standard-conforming Fortran95
- [ ] **Documentation**: Complete API docs and user guide

---

## ⚡ QUICK WINS (Pick These First)

1. **Split `ast_core.f90`** - Immediate architecture improvement
2. **Fix function parsing** - Unlocks many failing tests
3. **Add undeclared variable detection** - Easy semantic enhancement
4. **Implement basic scoping** - Foundation for advanced features
5. **Fix implied DO loops** - Closes several codegen gaps

---

## 🚫 EXPLICITLY OUT OF SCOPE (For Now)

- Object-oriented programming features (Fortran 2003+)
- Coarrays and parallel constructs (Fortran 2008+) 
- Submodules (Fortran 2008+)
- Parameterized derived types (Fortran 2003+)
- Advanced optimization passes
- IDE integrations
- Language server protocol

---

**Last Updated**: 2025-01-30  
**Total Estimated Timeline**: 4-6 weeks for Phases I-III  
**Priority Focus**: Architecture → Functions → Scoping → Type System