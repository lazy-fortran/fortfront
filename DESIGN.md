# Automatic Function Variable Declaration Generation (Issue #320)

## Problem Analysis

**Current Issue**: Functions with undeclared variables produce invalid standard Fortran code. The semantic analyzer correctly infers types but the code generator doesn't emit explicit variable declarations for function-scoped variables, particularly result variables.

**Example Problem**:
```fortran
! Input (lazy Fortran)
function twice(x) result(y)
y = 2*x
end function

! Current Output (INVALID - missing y declaration)
function twice(x) result(y)
    implicit none
    real(8), intent(in) :: x
    y = 2*x
end function twice

! Required Output (VALID - complete declarations)
function twice(x) result(y)
    implicit none
    real(8), intent(in) :: x
    real(8) :: y
    y = 2*x
end function twice
```

## Root Cause Analysis

### Current Architecture Gaps

1. **Standardizer Limitation**: The `standardizer.f90` module only handles `program_node` entries, not `function_def_node` or `subroutine_def_node`
2. **Code Generation Gap**: The `codegen_core.f90` generates parameter declarations but not local/result variable declarations
3. **Semantic Integration**: Type inference works correctly but doesn't trigger declaration generation for functions

### Existing Infrastructure Analysis

**✅ Available Components**:
- **Type Inference**: `semantic_analyzer.f90` correctly infers all variable types including result variables
- **Declaration Infrastructure**: `standardizer.f90` has comprehensive declaration generation for programs
- **AST Storage**: Inferred types are stored in `node%inferred_type` for all variables

**❌ Missing Components**:
- **Function Standardization**: No equivalent to `insert_variable_declarations` for functions
- **Function Variable Collection**: No function-specific variable discovery logic
- **Result Variable Handling**: No special handling for function result variables

## Solution Architecture

### Phase 1: Extend Standardizer for Functions

**Objective**: Add function-specific variable declaration generation to the standardizer

**Key Changes**:
1. **Function Standardization Entry Point**: Add `standardize_function_def` and `standardize_subroutine_def`
2. **Function Variable Collection**: Implement `collect_function_variables` to find undeclared variables
3. **Result Variable Detection**: Special logic for function result variables
4. **Integration**: Call function standardization from main `standardize_ast`

**Implementation Points**:
- Add function handlers in `standardize_ast` main dispatch
- Create `insert_function_variable_declarations` parallel to existing program logic
- Handle function scope vs program scope properly
- Preserve existing parameter declarations from semantic analyzer

### Phase 2: Function Variable Discovery

**Objective**: Collect all variables used in function bodies that need declarations

**Key Changes**:
1. **Variable Scanning**: Traverse function body to find all variable usage
2. **Declaration Filtering**: Skip variables that already have explicit declarations
3. **Result Variable Priority**: Always declare result variables even if not explicitly used
4. **Parameter Exclusion**: Don't redeclare parameters (already handled)

**Implementation Points**:
- Reuse `collect_statement_vars` logic from standardizer
- Add `collect_function_result_var` for result variable handling
- Filter against existing `declaration_node` and `parameter_declaration_node` entries
- Handle both named result variables and implicit function name results

### Phase 3: Type-Based Declaration Generation

**Objective**: Generate properly typed variable declarations from inferred types

**Key Changes**:
1. **Type Translation**: Convert `mono_type_t` to Fortran declaration strings
2. **Result Variable Typing**: Use function result type or inferred type
3. **Declaration Creation**: Generate `declaration_node` entries in AST
4. **Position Management**: Insert declarations after `implicit none`

**Implementation Points**:
- Extend existing `create_declaration_from_type` logic
- Handle character length specifications properly
- Support array declarations with inferred dimensions
- Generate clean, standard-compliant declaration syntax

## Detailed Technical Specifications

### 1. Function Standardization Integration

**Location**: `src/standardizer.f90`

```fortran
! Add to standardize_ast main dispatch
type is (function_def_node)
    call standardize_function_def(arena, root_index, func_def)
type is (subroutine_def_node)  
    call standardize_subroutine_def(arena, root_index, sub_def)
```

**New Functions**:
```fortran
subroutine standardize_function_def(arena, func_index, func_def)
    ! 1. Insert implicit none if missing
    ! 2. Collect undeclared variables in function body
    ! 3. Generate variable declarations from inferred types
    ! 4. Insert declarations after implicit none
end subroutine

subroutine insert_function_variable_declarations(arena, func_def, func_index)
    ! Parallel to insert_variable_declarations but for functions
    ! Handle function-specific scoping and result variables
end subroutine
```

### 2. Function Variable Collection Algorithm

**Process**:
1. **Scan Function Body**: Traverse `func_def%body_indices` to find variable usage
2. **Extract Result Variable**: From `func_def%result_variable` or function name
3. **Filter Declared Variables**: Check against existing declarations in body
4. **Build Variable List**: Create list of undeclared variables needing declarations

**Special Cases**:
- **Result Variable**: Always include even if not used (required by Fortran standard)
- **Parameter Variables**: Exclude from declaration generation (handled by semantic analyzer)
- **Local Variables**: Include all variables used in assignments or expressions

### 3. Type-Driven Declaration Generation

**Type Mapping Logic**:
```fortran
function mono_type_to_fortran_decl(mono_type) result(decl_string)
    select case (mono_type%kind)
    case (TINT)
        decl_string = "integer"
    case (TREAL)  
        decl_string = "real(8)"  ! Default real precision
    case (TCHAR)
        if (mono_type%char_size > 0) then
            decl_string = "character(len=" // trim(int_to_string(mono_type%char_size)) // ")"
        else
            decl_string = "character(len=:), allocatable"
        end if
    case (TLOGICAL)
        decl_string = "logical"
    case (TARRAY)
        ! Handle array declarations with proper dimensions
    end select
end function
```

### 4. Declaration Insertion Strategy

**Position Logic**:
1. **Find Insertion Point**: After `implicit none`, before first executable statement
2. **Group Declarations**: Group by type for cleaner output
3. **Preserve Order**: Maintain logical declaration order (parameters, locals, result)
4. **Update Body Indices**: Properly insert new declaration nodes in function body

## Implementation Plan

### Step 1: Function Standardization Framework (45 min)
- Add function handlers to `standardize_ast` dispatch
- Create `standardize_function_def` and `standardize_subroutine_def` stubs  
- Implement basic function body traversal
- Add integration tests for function standardization

### Step 2: Variable Collection Logic (1 hour)
- Implement `collect_function_variables` reusing existing logic
- Add `collect_function_result_var` for result variable handling
- Create filtering logic for existing declarations
- Test variable discovery on simple functions

### Step 3: Declaration Generation (1 hour)
- Extend `mono_type_to_fortran_decl` type conversion
- Implement declaration node creation for functions
- Add declaration insertion logic after `implicit none`
- Handle multi-variable declarations properly

### Step 4: Integration and Testing (45 min)
- Test complete pipeline: semantic analysis → standardization → code generation
- Verify Issue #320 example produces correct output
- Check edge cases: no result variable, complex types, nested functions
- Ensure no regressions in existing program standardization

### Step 5: Cleanup and Optimization (30 min)
- Remove any debugging code
- Optimize variable collection performance
- Add comprehensive error handling
- Update documentation and code comments

## Risk Assessment

### High Risk
- **AST Corruption**: Modifying function bodies could break existing nodes
- **Mitigation**: Use arena-safe operations, validate node indices, comprehensive testing

### Medium Risk  
- **Type System Integration**: Converting inferred types to declarations
- **Mitigation**: Reuse proven type conversion logic, test with all type variants

### Low Risk
- **Performance Impact**: Additional standardization pass for functions
- **Mitigation**: Only process functions that need declarations, optimize collection

## Success Criteria

1. **Issue #320 Example Works**: The exact example from Issue #320 produces correct output
2. **All Variable Types Supported**: Integer, real, character, logical, array declarations
3. **Result Variables Always Declared**: Function result variables always get declarations
4. **No Regressions**: Existing program standardization continues to work
5. **Clean Code Generation**: Generated declarations are properly formatted and standard-compliant
6. **Edge Cases Handled**: Functions without result variables, functions with explicit declarations

## Testing Strategy

### Unit Tests
- Function variable collection
- Type-to-declaration conversion  
- Declaration insertion logic
- Result variable detection

### Integration Tests
- Complete Issue #320 example pipeline
- Functions with different variable types
- Functions with mixed declared/undeclared variables
- Functions with complex result types

### Regression Tests
- All existing standardizer tests
- All existing semantic analysis tests
- Full fortfront pipeline tests

---

# Character Type Inference Architecture (Issue #329)

## Problem Analysis

**Current Issue**: Character type inference has multiple limitations causing test failures and incorrect type defaults in the semantic analysis pipeline:

### Core Problems Identified

1. **Function Parameter Inference Failure** (test_issue_312)
   - Character parameters in functions default to `real(8)` instead of character
   - String concatenation operations (`//`) on parameters don't trigger character inference
   - Example: `function concat_hello(x); concat_hello = x // "!"` - parameter `x` should be character

2. **Semantic Preservation Issues** (test_semantic_preservation)
   - Scope preservation failing for character variables
   - Expression order not maintained in character operations
   - Declaration consolidation breaks with character types

3. **Type System Limitations**
   - Character literal inference works but doesn't propagate through expressions
   - Binary operator `//' only handles already-typed character operands
   - No backward propagation from character operations to untyped variables

## Root Cause Analysis

### Type Inference Flow Issues

1. **Unidirectional Inference**: Current system infers types forward (literals → expressions) but lacks backward propagation (operations → parameters)

2. **Default Type Assignment**: Untyped variables default to `real(8)` before context analysis

3. **Operation Context Loss**: String concatenation operator doesn't provide type hints to untyped operands

4. **Scope Chain Breaks**: Character type information lost when crossing scope boundaries

## Solution Architecture

### Phase 1: Enhanced Character Type Propagation

**Objective**: Implement backward type propagation from operations to parameters

**Key Changes**:
1. **Pre-Analysis Pass**: Scan for character operations before type defaulting
2. **Context-Aware Inference**: Use operation context to infer parameter types
3. **Deferred Defaulting**: Only apply `real(8)` default after exhausting inference

**Implementation Points**:
- Modify `infer_function` in `semantic_analyzer.f90`
- Add character operation detection before parameter type assignment
- Track character operations in function bodies during first pass

### Phase 2: Binary Operation Type Hints

**Objective**: Make `//' operator provide strong type hints to operands

**Key Changes**:
1. **Type Constraint Collection**: Gather constraints from operations before unification
2. **Bidirectional Unification**: Allow operations to constrain operand types
3. **Character Operation Registry**: Track all character operations for constraint solving

**Implementation Points**:
- Enhance `infer_binary_op` to collect type constraints
- Add constraint solver phase before type defaulting
- Implement character type propagation through expression trees

### Phase 3: Scope Preservation for Character Types

**Objective**: Maintain character type information across scopes

**Key Changes**:
1. **Scope Type Cache**: Preserve inferred types when entering/exiting scopes
2. **Character Type Persistence**: Ensure character types survive scope transitions
3. **Declaration Consolidation**: Special handling for character declarations

**Implementation Points**:
- Fix scope manager to preserve character type attributes
- Enhance declaration processing for character variables
- Maintain type consistency across scope boundaries

## Detailed Technical Specifications

### 1. Function Parameter Character Inference

```fortran
! Current behavior (WRONG):
function concat_hello(x)  ! x defaults to real(8)
    concat_hello = x // "!"  ! Type error or incorrect handling
end function

! Target behavior (CORRECT):
function concat_hello(x)  ! x inferred as character from usage
    character(len=:), allocatable :: x  ! or character(*)
    concat_hello = x // "!"
end function
```

**Algorithm**:
1. First pass: Collect all operations involving parameters
2. Identify character operations (`//`, character function calls)
3. Mark parameters used in character contexts
4. Apply character type before defaulting to real(8)

### 2. Character Operation Constraint Collection

**Data Structure**:
```fortran
type :: type_constraint_t
    integer :: var_index
    integer :: constraint_kind  ! MUST_BE_CHARACTER, etc.
    integer :: source_location  ! Where constraint originated
end type
```

**Process**:
1. During AST traversal, collect constraints
2. Solve constraints before type defaulting
3. Apply solved types to variables

### 3. Scope Preservation Enhancement

**Issue**: Character types lost during scope transitions

**Solution**:
- Add character type preservation to scope manager
- Implement proper type copying for character attributes
- Ensure allocatable info preserved across scopes

## Implementation Plan

### Step 1: Test Infrastructure (30 min)
- Create focused test cases for each failure mode
- Add regression tests for character inference
- Implement test helpers for type checking

### Step 2: Constraint Collection (1 hour)
- Add constraint data structures
- Implement constraint collection in semantic analyzer
- Create constraint solver for character types

### Step 3: Parameter Inference (1 hour)
- Modify function analysis to use constraints
- Implement backward propagation from operations
- Update parameter type assignment logic

### Step 4: Scope Preservation (45 min)
- Fix scope manager character handling
- Ensure type attributes preserved
- Update declaration consolidation

### Step 5: Integration Testing (45 min)
- Run full test suite
- Verify all 6 character tests pass
- Check for regressions

## Risk Assessment

### High Risk
- **Type System Regression**: Changes might affect non-character types
- **Mitigation**: Comprehensive regression testing, isolated changes

### Medium Risk
- **Performance Impact**: Additional analysis passes
- **Mitigation**: Optimize constraint collection, cache results

### Low Risk
- **Backward Compatibility**: Should be transparent to users
- **Mitigation**: Preserve existing API, internal changes only

## Success Criteria

1. **All 6 character-related semantic tests pass**
2. **No regression in existing tests**
3. **Function parameters correctly inferred as character**
4. **Scope preservation maintains character types**
5. **Expression order preserved for character operations**
6. **Declaration consolidation works with character types**

## Testing Strategy

### Unit Tests
- Character literal inference
- Character concatenation operations
- Function parameter inference
- Scope preservation

### Integration Tests
- Full program character handling
- Mixed type programs
- Complex character expressions

### Regression Tests
- All existing semantic tests
- Frontend integration tests
- Code generation verification

---

# Input Validation Module Architecture (Issue #262)

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