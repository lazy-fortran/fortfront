# Automatic Variable Declaration Generation Architecture (Issue #320)

## Problem Analysis

**Current Issue**: Functions with undeclared variables are translated without proper type declarations, resulting in invalid Fortran code that fails compilation.

**Example Problem**:
```fortran
! Input (lazy Fortran)
function twice(x) result(y)
    y = 2*x
end function

! Current Output (invalid Fortran)
function twice(x) result(y)
    implicit none
    y = 2*x  ! ERROR: x and y are undeclared
end function twice
```

**Required Solution**: Automatically generate appropriate variable declarations for undeclared variables in functions through:
1. **Variable Detection**: Identify all undeclared variables in function scopes
2. **Type Inference**: Determine types through semantic analysis and type inference
3. **Declaration Generation**: Create proper declaration nodes and insert into AST
4. **Error Handling**: Report failures when types cannot be inferred rather than defaulting to `real`

## Architecture Overview

### Core Components

#### 1. Variable Declaration Analyzer (`variable_declaration_analyzer.f90`)
**Purpose**: Dedicated semantic analyzer for automatic variable declaration generation
**Base Class**: `semantic_analyzer_t` from semantic pipeline
**Integration**: Plugs into existing semantic analysis pipeline

#### 2. Undeclared Variable Detector  
**Purpose**: Traverse function AST to identify variables used but not declared
**Scope**: Function-level analysis only (preserves program-level implicit behavior)
**Method**: Symbol table comparison between declared and used variables

#### 3. Type Inference Integration
**Purpose**: Leverage existing Hindley-Milner type system for accurate type determination
**Integration**: Uses `semantic_context_t` type inference results
**Fallback**: Structured error reporting when types cannot be determined

#### 4. Declaration Insertion Strategy
**Purpose**: Modify function AST to include generated declarations
**Location**: Insert immediately after `implicit none` statement
**Method**: AST factory functions to create `declaration_node` instances

## Detailed Design

### Variable Declaration Analyzer Architecture

```fortran
module variable_declaration_analyzer
    use semantic_analyzer_base, only: semantic_analyzer_t
    use ast_core, only: ast_arena_t
    use semantic_analyzer, only: semantic_context_t
    implicit none
    private
    
    public :: variable_declaration_analyzer_t
    
    type, extends(semantic_analyzer_t) :: variable_declaration_analyzer_t
        ! Configuration
        logical :: strict_mode = .true.  ! Error on uninferable types
    contains
        procedure :: analyze => analyze_variable_declarations
        procedure :: get_results => get_declaration_results  
        procedure :: assign => assign_analyzer
        procedure :: get_dependencies => get_analyzer_dependencies
        procedure :: get_name => get_analyzer_name
    end type
end module
```

### Core Analysis Pipeline

#### Phase 1: Function Scope Detection
```fortran
function collect_function_scopes(arena, root_index) result(function_indices)
    type(ast_arena_t), intent(in) :: arena
    integer, intent(in) :: root_index
    integer, allocatable :: function_indices(:)
    
    ! Traverse AST to find all function_def_node instances
    ! Return array of arena indices for function definitions
end function
```

#### Phase 2: Variable Collection  
```fortran
function collect_undeclared_variables(arena, func_index, context) result(variables)
    type(ast_arena_t), intent(in) :: arena
    integer, intent(in) :: func_index
    type(semantic_context_t), intent(in) :: context
    type(variable_info_t), allocatable :: variables(:)
    
    ! For each function:
    ! 1. Collect all declared variables (parameters + declarations)
    ! 2. Collect all used variables (identifiers in expressions)
    ! 3. Return difference set (used but not declared)
end function
```

#### Phase 3: Type Inference Application
```fortran
function infer_variable_types(variables, context) result(typed_variables)
    type(variable_info_t), intent(in) :: variables(:)
    type(semantic_context_t), intent(in) :: context
    type(typed_variable_t), allocatable :: typed_variables(:)
    
    ! For each undeclared variable:
    ! 1. Query semantic context for inferred type
    ! 2. Convert mono_type_t to Fortran type string
    ! 3. Handle type inference failures with structured errors
end function
```

#### Phase 4: Declaration Generation
```fortran
function generate_variable_declarations(arena, func_index, typed_vars) result(success)
    type(ast_arena_t), intent(inout) :: arena  
    integer, intent(in) :: func_index
    type(typed_variable_t), intent(in) :: typed_vars(:)
    logical :: success
    
    ! For each typed variable:
    ! 1. Create declaration_node using AST factory
    ! 2. Insert into function body after implicit none
    ! 3. Update function body_indices array
end function
```

### Data Types

#### Variable Information Storage
```fortran
type :: variable_info_t
    character(len=:), allocatable :: name
    integer :: first_use_index     ! AST index of first usage
    integer :: line, column        ! Source location
end type

type :: typed_variable_t  
    character(len=:), allocatable :: name
    character(len=:), allocatable :: fortran_type  ! "real", "integer", etc.
    integer :: kind_value = 0      ! Kind parameter
    logical :: has_kind = .false.
    logical :: type_inferred = .false.  ! Success flag
    character(len=:), allocatable :: error_msg  ! If type_inferred = .false.
end type
```

### Integration with Semantic Pipeline

#### Registration in Pipeline
```fortran
! In semantic_pipeline.f90 or semantic_analyzer.f90
subroutine register_variable_declaration_analyzer(pipeline)
    type(semantic_pipeline_t), intent(inout) :: pipeline
    type(variable_declaration_analyzer_t) :: analyzer
    
    ! Configure analyzer
    analyzer%strict_mode = .true.
    
    ! Register with pipeline
    call pipeline%register_analyzer(analyzer)
end subroutine
```

#### Pipeline Dependencies
- **Prerequisites**: Type inference must complete before variable declaration analysis
- **Dependencies**: `["symbol_analyzer", "type_analyzer", "scope_analyzer"]`
- **Execution Order**: After core semantic analysis, before code generation

#### Integration with Existing Analyzers
Following the established pattern in `builtin_analyzers.f90`:

```fortran
! In builtin_analyzers.f90 - add to public declarations
public :: variable_declaration_analyzer_t

! Variable declaration analyzer - generates missing declarations
type, extends(semantic_analyzer_t) :: variable_declaration_analyzer_t
    type(variable_declaration_result_t) :: result
    logical :: analysis_complete = .false.
    logical :: strict_mode = .true.  ! Error on uninferable types
contains
    procedure :: analyze => analyze_variable_declarations
    procedure :: get_results => get_declaration_results
    procedure :: get_name => get_declaration_analyzer_name
    procedure :: assign => assign_declaration_analyzer  
    procedure :: get_dependencies => get_declaration_dependencies
    procedure :: reset_state => reset_declaration_state
end type
```

#### Result Type Definition
```fortran
type :: variable_declaration_result_t
    type(typed_variable_t), allocatable :: generated_declarations(:)
    integer, allocatable :: function_indices(:)  ! Functions modified
    logical :: success = .false.
    character(len=:), allocatable :: error_message
contains
    procedure :: assign_declaration_result
    generic :: assignment(=) => assign_declaration_result
end type
```

### Error Handling Strategy

#### Principle: Fail Fast with Clear Messages
Following Issue #320 comments requirement: "throw an error if types are not declared in a function or subroutine header"

```fortran
! When type inference fails:
type(result_t) :: validation_result

if (.not. typed_var%type_inferred) then
    validation_result = create_error_result( &
        "Cannot infer type for variable '" // typed_var%name // &
        "' in function '" // function_name // "'", &
        ERROR_TYPE_SYSTEM, &
        component="variable_declaration_analyzer", &
        suggestion="Add explicit type declaration for this variable", &
        line=typed_var%line, &
        column=typed_var%column &
    )
    return
end if
```

#### Error Categories
1. **Type Inference Failure**: Variable used but type cannot be determined
2. **Scope Conflict**: Variable declared in parameter list but also used locally  
3. **Declaration Insertion**: AST modification failure during declaration insertion
4. **Multiple Usage**: Variable used with conflicting inferred types

### AST Modification Strategy

#### Declaration Insertion Logic
```fortran
! Target AST structure:
function func_name(params) result(res)
    implicit none
    ! INSERT GENERATED DECLARATIONS HERE
    <existing function body>
end function
```

#### Implementation Approach
1. **Locate Insertion Point**: Find `implicit none` statement index in function body
2. **Create Declaration Nodes**: Use `create_declaration()` factory function
3. **Update Body Indices**: Rebuild `body_indices` array to include new declarations
4. **Preserve Order**: Maintain original statement ordering

#### Memory Management
- **Arena Integration**: All new declarations added to existing AST arena
- **Index Management**: Update all parent/child index relationships  
- **Cleanup**: No manual memory management needed (arena handles deallocation)

### Detailed Implementation Algorithms

#### Variable Collection Algorithm
```fortran
function collect_undeclared_variables(arena, func_index, context) result(variables)
    type(ast_arena_t), intent(in) :: arena
    integer, intent(in) :: func_index
    type(semantic_context_t), intent(in) :: context
    type(variable_info_t), allocatable :: variables(:)
    
    character(len=:), allocatable :: declared_vars(:)
    character(len=:), allocatable :: used_vars(:)
    character(len=:), allocatable :: undeclared_vars(:)
    
    ! Step 1: Collect declared variables
    declared_vars = get_declared_variables(arena, func_index)
    
    ! Step 2: Collect used variables  
    used_vars = get_used_variables(arena, func_index)
    
    ! Step 3: Find difference (used but not declared)
    undeclared_vars = set_difference(used_vars, declared_vars)
    
    ! Step 4: Create variable_info_t array with location data
    allocate(variables(size(undeclared_vars)))
    do i = 1, size(undeclared_vars)
        variables(i)%name = undeclared_vars(i)
        variables(i)%first_use_index = find_first_usage(arena, func_index, undeclared_vars(i))
        call get_source_location(arena, variables(i)%first_use_index, &
                                 variables(i)%line, variables(i)%column)
    end do
end function

function get_declared_variables(arena, func_index) result(declared_vars)
    type(ast_arena_t), intent(in) :: arena
    integer, intent(in) :: func_index
    character(len=:), allocatable :: declared_vars(:)
    
    ! Extract function node
    select type(func_node => arena%entries(func_index)%node)
    type is (function_def_node)
        character(len=:), allocatable :: param_names(:)
        character(len=:), allocatable :: local_vars(:)
        
        ! Get parameter names
        param_names = extract_parameter_names(arena, func_node%param_indices)
        
        ! Get local declaration names
        local_vars = extract_declaration_names(arena, func_node%body_indices)
        
        ! Combine parameter and local variable names
        declared_vars = concatenate_arrays(param_names, local_vars)
    end select
end function

function get_used_variables(arena, func_index) result(used_vars)
    type(ast_arena_t), intent(in) :: arena  
    integer, intent(in) :: func_index
    character(len=:), allocatable :: used_vars(:)
    
    ! Extract function node
    select type(func_node => arena%entries(func_index)%node)
    type is (function_def_node)
        character(len=:), allocatable :: identifiers(:)
        
        ! Traverse function body to collect all identifier nodes
        identifiers = collect_identifier_names(arena, func_node%body_indices)
        
        ! Filter out built-in functions and remove duplicates
        used_vars = filter_user_variables(identifiers)
    end select  
end function
```

#### Scope-aware Type Inference Query
```fortran
function query_inferred_type(context, var_name, func_scope) result(inferred_type)
    type(semantic_context_t), intent(in) :: context
    character(len=*), intent(in) :: var_name
    integer, intent(in) :: func_scope  ! AST index of function
    type(mono_type_t) :: inferred_type
    
    ! Query the hierarchical scope system for the variable's type
    ! within the function's scope context
    call context%scopes%push_scope()  ! Enter function scope
    
    if (context%scopes%lookup(var_name, poly_scheme)) then
        ! Type found in scope - extract monotype
        inferred_type = instantiate_poly_type(poly_scheme)
    else
        ! Type not found - check if it's in the type environment
        ! from the semantic analysis phase
        inferred_type = lookup_in_type_env(context%env, var_name)
    end if
    
    call context%scopes%pop_scope()   ! Exit function scope
end function
```

#### Declaration Insertion with Arena Index Management
```fortran
function insert_declarations_after_implicit(arena, func_index, declarations) result(success)
    type(ast_arena_t), intent(inout) :: arena
    integer, intent(in) :: func_index
    type(typed_variable_t), intent(in) :: declarations(:)
    logical :: success
    
    integer :: implicit_none_index, insertion_point
    integer, allocatable :: new_body_indices(:)
    integer, allocatable :: declaration_indices(:)
    
    ! Step 1: Find implicit none statement
    implicit_none_index = find_implicit_none_statement(arena, func_index)
    if (implicit_none_index <= 0) then
        success = .false.
        return
    end if
    
    ! Step 2: Create declaration nodes in arena
    allocate(declaration_indices(size(declarations)))
    do i = 1, size(declarations)
        declaration_indices(i) = create_declaration_in_arena(arena, declarations(i))
    end do
    
    ! Step 3: Rebuild function body_indices array
    select type(func_node => arena%entries(func_index)%node)
    type is (function_def_node)
        integer :: original_size, new_size
        integer :: insertion_pos
        
        original_size = size(func_node%body_indices)
        new_size = original_size + size(declarations)
        
        ! Find position after implicit none
        insertion_pos = find_array_position(func_node%body_indices, implicit_none_index) + 1
        
        ! Create new body_indices array with declarations inserted
        allocate(new_body_indices(new_size))
        
        ! Copy elements before insertion point
        new_body_indices(1:insertion_pos-1) = func_node%body_indices(1:insertion_pos-1)
        
        ! Insert declaration indices
        new_body_indices(insertion_pos:insertion_pos+size(declarations)-1) = declaration_indices
        
        ! Copy remaining elements after insertion point
        new_body_indices(insertion_pos+size(declarations):) = &
            func_node%body_indices(insertion_pos:)
        
        ! Update function node
        call move_alloc(new_body_indices, func_node%body_indices)
        success = .true.
    end select
end function
```

### Type System Integration

#### Hindley-Milner Type Mapping
```fortran
function mono_type_to_fortran(mono_type) result(fortran_type)
    type(mono_type_t), intent(in) :: mono_type
    character(len=:), allocatable :: fortran_type
    
    select case (mono_type%kind)
    case (TINT)
        fortran_type = "integer"
    case (TREAL) 
        fortran_type = "real"
    case (TLOGICAL)
        fortran_type = "logical"
    case (TCHAR)
        fortran_type = "character(len=*)"
    case (TARRAY)
        ! Handle array types with proper dimensions
        fortran_type = mono_type_to_fortran(mono_type%element_type) // ", allocatable"
    case default
        ! Type inference failure
        fortran_type = ""
    end select
end function
```

#### Kind Parameter Handling
- **Default Behavior**: Use compiler default kinds
- **Explicit Kinds**: Preserve when inferred from context
- **Configuration**: Allow user-specified default kinds

### Code Generation Integration

#### Codegen Modifications
**No changes required** - existing code generation already handles `declaration_node` instances correctly.

#### Output Format
```fortran
! Generated output example:
function calc(x, y) result(res)
    implicit none
    real :: x           ! Generated from type inference
    real :: y           ! Generated from type inference  
    real :: res         ! Generated from type inference
    real :: temp        ! Generated from type inference
    
    res = x + y + temp
    temp = 5.0
end function calc
```

## Implementation Plan

### Phase 1: Core Analyzer Development (Priority 1)
1. **Create `variable_declaration_analyzer.f90`**
   - Extend `semantic_analyzer_t` base class
   - Implement basic analyzer structure following existing patterns
   - Add to `builtin_analyzers.f90` public interface

2. **Implement Core Collection Functions**
   - `collect_undeclared_variables()` - main analysis function
   - `get_declared_variables()` - extract parameter and local declarations
   - `get_used_variables()` - traverse AST for identifier usage
   - `set_difference()` - utility for finding undeclared variables

3. **Implement Type Inference Integration**
   - `query_inferred_type()` - query semantic context for variable types
   - `mono_type_to_fortran()` - convert type system types to Fortran strings
   - `infer_variable_types()` - process undeclared variables through type system

4. **Add Comprehensive Unit Tests**
   - Test individual functions with mock AST data
   - Verify variable collection accuracy
   - Test type inference integration
   - Validate error handling for edge cases

### Phase 2: AST Modification System (Priority 1)
1. **Implement Declaration Generation**
   - `create_declaration_in_arena()` - factory function integration
   - `generate_variable_declarations()` - main declaration creation logic
   - Integration with existing `declaration_node` infrastructure

2. **Implement Declaration Insertion Logic**
   - `find_implicit_none_statement()` - locate insertion point
   - `insert_declarations_after_implicit()` - modify function body_indices
   - `rebuild_function_body()` - maintain AST index consistency

3. **Update AST Factory Integration**
   - Ensure `create_declaration()` works with generated types
   - Test arena memory management with inserted nodes
   - Validate parent/child relationships after insertion

4. **Test AST Consistency After Modifications**
   - Verify arena integrity after declarations added
   - Test JSON serialization of modified AST
   - Validate traversal algorithms with new structure

### Phase 3: Pipeline Integration (Priority 2)
1. **Register Analyzer in Semantic Pipeline**
   - Add to `builtin_analyzers.f90` exports
   - Configure dependencies: `["symbol_analyzer", "type_analyzer", "scope_analyzer"]`
   - Set execution order: after type inference, before code generation

2. **Implement Result Integration**
   - Define `variable_declaration_result_t` type
   - Implement assignment operators following existing patterns
   - Add result serialization for debugging/inspection

3. **Add End-to-End Integration Tests**
   - Test full compilation pipeline with undeclared variables
   - Verify generated Fortran compiles correctly
   - Test integration with existing test suite

4. **Validate Complex Function Scenarios**
   - Multiple functions with different variable patterns
   - Nested function calls with type dependencies
   - Mixed declared/undeclared variable scenarios

### Phase 4: Error Handling Enhancement (Priority 2)
1. **Implement Structured Error Reporting**
   - Use `result_t` pattern from error handling system
   - Create specific error categories for type inference failures
   - Add source location information to error messages

2. **Add Helpful Error Messages and Suggestions**
   - Clear messages when types cannot be inferred
   - Suggestions for explicit type declarations
   - Context-aware error descriptions

3. **Create Error Recovery Mechanisms**
   - Graceful handling of partial type inference failures
   - Continue processing other variables when some fail
   - Maintain compilation pipeline stability

4. **Test Error Scenarios Comprehensively**
   - Variables with uninferable types
   - Conflicting type usage patterns
   - Invalid function structures
   - Edge cases in type system interactions

### Phase 5: Documentation and Quality Assurance (Priority 3)
1. **Update Technical Documentation**
   - API documentation for new analyzer
   - Integration guide for semantic pipeline
   - Examples of generated declaration patterns

2. **Performance Optimization**
   - Profile variable collection algorithms
   - Optimize AST traversal for large functions
   - Memory usage analysis and optimization

3. **Integration Testing**
   - Test with real-world Fortran code samples
   - Validate against different function complexity levels
   - Ensure compatibility with existing fortfront features

4. **Final Quality Review**
   - Code review following SOLID principles
   - Memory safety validation
   - Performance benchmarking

## Quality Assurance Requirements

### Test Coverage Targets
- **Unit Tests**: Individual analyzer functions (90%+ coverage)
- **Integration Tests**: Full pipeline with various function types
- **Error Tests**: All error scenarios with proper messages
- **Regression Tests**: Ensure existing functionality unaffected

### Test Scenarios
1. **Simple Functions**: Single undeclared variable with clear type
2. **Complex Functions**: Multiple variables with different types
3. **Mixed Scenarios**: Some declared, some undeclared variables
4. **Error Cases**: Variables with uninferable types
5. **Edge Cases**: Empty functions, parameter conflicts, recursive usage

### Performance Requirements
- **Analysis Time**: <50ms for typical function (10-20 variables)
- **Memory Usage**: <1MB additional memory per analyzed function
- **Scalability**: Linear complexity with number of variables

## Success Criteria

### Functional Requirements
1. **✅ Automatic Declaration**: Functions with undeclared variables get proper declarations
2. **✅ Type Accuracy**: Generated types match semantic analysis results  
3. **✅ Error Reporting**: Clear errors when types cannot be inferred
4. **✅ Non-Breaking**: Existing functionality unaffected

### Quality Requirements  
1. **✅ Zero Test Regressions**: All existing tests continue to pass
2. **✅ Clean Integration**: Follows existing semantic pipeline patterns
3. **✅ Comprehensive Testing**: New functionality thoroughly tested
4. **✅ Performance**: No significant slowdown in compilation pipeline

### User Experience Requirements
1. **✅ Transparent Operation**: Works automatically without user intervention
2. **✅ Helpful Errors**: Clear guidance when manual declarations needed
3. **✅ Predictable Behavior**: Consistent type inference across similar cases
4. **✅ Standard Compliance**: Generated code compiles with standard Fortran compilers

## Executive Summary

### Architecture Decision Summary

**Problem Solved**: Automatic generation of missing variable declarations in functions to transform lazy Fortran into standard-compliant code.

**Core Design Decisions**:

1. **Semantic Pipeline Integration**: Implemented as a `semantic_analyzer_t` plugin following established patterns in `builtin_analyzers.f90`
2. **Type System Leverage**: Directly integrates with existing Hindley-Milner type inference for accurate type determination  
3. **AST Arena Modification**: Uses in-place AST modification with proper index management for declaration insertion
4. **Error-First Approach**: Fails fast with clear error messages when types cannot be inferred (no defaulting to `real`)
5. **Function-Scope Only**: Preserves program-level implicit behavior, only processes function definitions

**Key Technical Innovations**:

- **Variable Collection Algorithm**: Set-based difference operation between declared and used variables
- **Scope-Aware Type Query**: Leverages hierarchical scope system for accurate type resolution
- **Declaration Insertion Strategy**: AST modification with proper arena index management
- **Pipeline Dependencies**: Executes after type inference but before code generation

**Integration Points**:
- **Input**: Function AST nodes with undeclared variables  
- **Processing**: Semantic context with type inference results
- **Output**: Modified AST with generated `declaration_node` instances
- **Error Handling**: Structured error reporting following `result_t` pattern

**Quality Assurance**:
- **Zero Breaking Changes**: Existing functionality completely preserved
- **Comprehensive Testing**: Unit, integration, and end-to-end test coverage  
- **Performance Optimization**: Linear complexity with number of variables
- **Memory Safety**: Arena-based memory management with automatic cleanup

### Implementation Strategy

**Incremental Development**: 5-phase implementation plan prioritizing core functionality first
**Risk Mitigation**: Extensive testing at each phase before proceeding
**Quality Gates**: Code review, performance validation, and architectural consistency checks

### Expected Outcomes

**For Users**:
- Seamless automatic variable declaration generation
- Clear error messages when manual intervention needed
- Standard-compliant Fortran output ready for compilation

**For Developers**:
- Clean semantic analyzer plugin following established patterns
- Extensible foundation for future lazy Fortran features
- Maintainable code with comprehensive test coverage

**For Architecture**:
- Demonstrates semantic pipeline extensibility
- Validates AST modification capabilities
- Establishes patterns for future transformation plugins

This architecture solves Issue #320 while maintaining fortfront's core principles of clean abstraction, performance optimization, and architectural consistency.

---

# Previous Architecture: Input Validation Module (Issue #262)

## Problem Analysis

**Current Issue**: Input validation logic is embedded within the `frontend.f90` module, creating tight coupling between validation concerns and the main transformation pipeline. This makes the validation logic:

- **Non-reusable**: Cannot be used independently for editor integration, build tools, or other applications
- **Hard to test**: Validation logic is tested indirectly through frontend transformation
- **Difficult to maintain**: Changes to validation affect the main frontend processing pipeline
- **Architecturally impure**: Violates single responsibility principle by mixing validation and transformation concerns

**Solution**: Extract validation logic into a dedicated `input_validation` module with clean separation of concerns and independent API.

## Module Architecture Design

### Input Validation Module (`src/input_validation.f90`)

**Purpose**: Dedicated module providing comprehensive input validation with enhanced error reporting, completely independent of frontend transformation logic.

**Dependencies**: 
- `lexer_core` only (for `token_t` type)
- No circular dependencies
- No dependency on `frontend` module

**Public API:**
```fortran
module input_validation
    use lexer_core, only: token_t
    implicit none
    private
    
    ! Primary validation interface
    public :: validate_basic_syntax
    
    ! Specific validation checks  
    public :: check_missing_then_statements
    public :: check_incomplete_statements
    public :: check_for_fortran_content
    public :: check_missing_end_constructs
    
    ! Utility functions
    public :: contains_invalid_patterns
    public :: has_only_meaningless_tokens
end module
```

### ✅ Current Validation Capabilities (Issue #256 Requirements)
- Enhanced error reporting with line/column information
- Source context with visual indicators pointing to errors  
- Helpful suggestions for fixing common syntax errors
- Elimination of silent fallback behavior
- Meaningful error output instead of empty programs
- Comprehensive syntax validation for all Fortran constructs

### 🎯 New Capabilities (Issue #262 Goals)
1. **Standalone Usage**: Validation independent of frontend transformation
2. **Editor Integration**: Real-time syntax checking capabilities
3. **Build Tool Integration**: Pre-compilation validation for build systems
4. **Educational Applications**: Teaching tools with immediate syntax feedback
5. **Code Quality Tools**: Lint-style checking functionality

## Design Principles

- **Single Responsibility**: Input validation module focuses solely on validation concerns
- **Clean Dependencies**: No circular dependencies or coupling with frontend transformation
- **Reusability**: Module API designed for multiple use cases (CLI, editor, build tools)
- **Backward Compatibility**: Existing frontend functionality remains unchanged
- **Error Quality**: Maintain Issue #256 high-quality error reporting standards
- **Testability**: Independent module enables direct testing of validation logic

## Implementation Plan

### Phase 1: Module Creation
1. **Extract Validation Functions**: Move all validation logic from `frontend.f90` to new `src/input_validation.f90`
2. **Define Clean Interface**: Create public API with clear function signatures
3. **Implement Error Formatting**: Extract and enhance error message formatting functions
4. **Remove Dependencies**: Ensure module only depends on `lexer_core`

### Phase 2: Frontend Integration  
1. **Update Frontend**: Modify `frontend.f90` to use new `input_validation` module
2. **Preserve Behavior**: Ensure existing functionality works identically
3. **Maintain Error Quality**: Keep all Issue #256 error reporting improvements
4. **Test Integration**: Verify all existing tests continue to pass

### Phase 3: Enhanced Capabilities
1. **Standalone Validation**: Enable independent usage without frontend
2. **Enhanced Error Context**: Improve error messages with better source context
3. **Performance Optimization**: Optimize validation for repeated use
4. **Documentation**: Create comprehensive usage examples and API documentation

## Validation Function Specifications

### Primary Interface: `validate_basic_syntax`

```fortran
subroutine validate_basic_syntax(source, tokens, error_msg)
    character(len=*), intent(in) :: source
    type(token_t), intent(in) :: tokens(:)
    character(len=:), allocatable, intent(out) :: error_msg
```

**Purpose**: Main validation entry point that orchestrates all validation checks  
**Behavior**: Calls specialized validation functions in logical order  
**Error Format**: Enhanced Issue #256 format with line/column/context/suggestions

### Specialized Validation Functions

#### `check_missing_then_statements`
```fortran
subroutine check_missing_then_statements(tokens, source_lines, error_msg)
```
**Detects**: `if` statements missing required `then` keyword  
**Example Error**: "Missing 'then' statement at line 1, column 9"

#### `check_incomplete_statements`  
```fortran
subroutine check_incomplete_statements(tokens, source_lines, error_msg)
```
**Detects**: Dangling operators, incomplete expressions  
**Example Error**: "Incomplete expression: dangling '+' operator at line 1"

#### `check_for_fortran_content`
```fortran
subroutine check_for_fortran_content(tokens, error_msg)
```
**Detects**: Input without recognizable Fortran patterns  
**Multi-Phase Logic**: Comments → Keywords → Expressions → Reject

#### `check_missing_end_constructs`
```fortran
subroutine check_missing_end_constructs(tokens, source_lines, error_msg)
```  
**Detects**: Program blocks without proper ending statements  
**Example Error**: "Missing 'end program' statement"

### Utility Functions

#### `contains_invalid_patterns`
```fortran
logical function contains_invalid_patterns(tokens)
```
**Returns**: True if tokens contain invalid syntax patterns

#### `has_only_meaningless_tokens`
```fortran
logical function has_only_meaningless_tokens(tokens)
```
**Returns**: True if input contains no meaningful Fortran content

## Integration with Existing System

### Frontend Module Changes
- **Import**: Add `use input_validation, only: validate_basic_syntax`
- **Replace**: Replace embedded validation logic with module calls
- **Preserve**: Maintain identical error reporting behavior
- **Cleanup**: Remove duplicated validation code

### Test Suite Compatibility
- **Existing Tests**: All current validation tests continue to pass
- **New Tests**: Additional tests for standalone module usage
- **Integration Tests**: Verify frontend still works with extracted module
- **Regression Testing**: Ensure Issue #256 requirements still met

### Performance Considerations
- **Module Loading**: Minimal overhead from additional module import
- **Function Calls**: Validation functions optimized for repeated use
- **Memory Usage**: No additional memory overhead compared to embedded logic
- **Compilation**: Clean module boundaries enable better compiler optimization

## Success Criteria

1. **✅ Functional Requirements**:
   - All existing validation behavior preserved
   - Module can be used independently of frontend
   - Enhanced error reporting maintains Issue #256 quality standards

2. **✅ Quality Requirements**:
   - Zero test regressions
   - Clean architectural separation
   - No circular dependencies
   - Comprehensive test coverage for standalone usage

3. **✅ Documentation Requirements**:
   - Clear API documentation with examples
   - Integration guide for external usage
   - Architectural documentation updated