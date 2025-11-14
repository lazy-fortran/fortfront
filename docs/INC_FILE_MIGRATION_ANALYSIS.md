# .inc File Investigation and Migration Analysis

## Executive Summary

**Total .inc files analyzed:** 29 (24 active, 5 orphaned)
**Orphaned files removed:** 5 (2,218 lines of dead code eliminated)
**Active .inc files remaining:** 24 files across 12 parent modules
**Total lines in active .inc files:** ~16,000 lines

## Key Findings

### 1. Orphaned Files (REMOVED - Dead Code)
- `src/frontend_parsing_boundary_detection.inc` (816 lines)
- `src/frontend_parsing_unit_detection.inc` (335 lines)
- `src/semantic/analyzers/semantic_analyzer_infer_type_locals_part1.inc` (494 lines)
- `src/semantic/analyzers/semantic_analyzer_infer_type_locals_part2.inc` (495 lines)
- `src/semantic/analyzers/semantic_analyzer_infer_type_locals_part3.inc` (78 lines)

**Total dead code removed: 2,218 lines**

These files were created in commit 0b86ac3e but never actually included in any parent module.

### 2. Active .inc Files by Category

#### Category A: Large Switch Statement Splits (Codegen)
**Pattern:** Large switch/select statements split across files to stay under 1000 lines

- `codegen_expressions.f90` (1,337 lines total)
  - `codegen_expressions_part1.inc` (826 lines) - Expression type handling part 1
  - `codegen_expressions_part2.inc` (511 lines) - Expression type handling part 2

- `codegen_statements.f90` (1,040 lines total)
  - `codegen_statements_part1.inc` (869 lines) - Statement type handling part 1
  - `codegen_statements_part2.inc` (171 lines) - Statement type handling part 2

- `codegen_function_declarations.f90` (1,345 lines total)
  - `codegen_function_declarations_part1.inc` (893 lines) - Function/subroutine code generation part 1
  - `codegen_function_declarations_part2.inc` (452 lines) - Function/subroutine code generation part 2

**Characteristics:**
- Giant select/case blocks on AST node types
- Minimal procedure boundaries - mainly one huge procedure split for readability
- Tightly coupled to parent module state

#### Category B: Large Switch Statement Splits (Parser)
- `parser_dispatcher.f90` (1,130 lines total)
  - `parser_dispatcher_part1.inc` (1,052 lines) - Main dispatch logic
  - `parser_dispatcher_part2.inc` (78 lines) - Additional dispatch cases

- `parser_do_constructs.f90` (1,078 lines total)
  - `parser_do_constructs_part1.inc` (884 lines) - Do loop parsing part 1
  - `parser_do_constructs_part2.inc` (194 lines) - Do loop parsing part 2

**Characteristics:** Similar to codegen - large switch statements

#### Category C: Module Procedure Decomposition
**Pattern:** Modules split into logical groups of related procedures

- `input_validation.f90` (1,161 lines total)
  - `input_validation_part1.inc` (894 lines) - Core validation procedures
  - `input_validation_part2.inc` (267 lines) - Helper functions and utilities

- `standardizer_types.f90` (1,028 lines total)
  - `standardizer_types_part1.inc` (891 lines) - Type standardization procedures part 1
  - `standardizer_types_part2.inc` (137 lines) - Type standardization procedures part 2

- `ast_monomorphization.f90` (2,565 lines total)
  - `ast_monomorphization_part1.inc` (864 lines) - Monomorphization logic part 1
  - `ast_monomorphization_part2.inc` (887 lines) - Monomorphization logic part 2
  - `ast_monomorphization_part3.inc` (814 lines) - Monomorphization logic part 3

**Characteristics:**
- Multiple independent procedures per file
- Clear public/private API boundaries
- Less coupled than switch statement splits

#### Category D: AST Factory/Nodes
- `ast_nodes_misc.f90` (1,493 lines total)
  - `ast_nodes_misc_part1.inc` (887 lines) - Miscellaneous AST node types part 1
  - `ast_nodes_misc_part2.inc` (606 lines) - Miscellaneous AST node types part 2

- `ast_factory_control.f90` (987 lines total)
  - `ast_factory_control_part1.inc` (786 lines) - Control flow node factory part 1
  - `ast_factory_control_part2.inc` (201 lines) - Control flow node factory part 2

#### Category E: Submodule Implementations
- `semantic_analyzer_infer_impl.f90` (submodule)
  - `semantic_analyzer_infer_impl_part1.inc` (22 lines) - Type inference implementation part 1
  - `semantic_analyzer_infer_impl_part2.inc` (53 lines) - Type inference implementation part 2
  - `semantic_analyzer_infer_impl_part3.inc` (150 lines) - Type inference implementation part 3

**Characteristics:** Already a submodule but still using .inc for internal split

### 3. Why .inc Files Were Used

**Achieved Goals:**
✅ Keep all files under 1000-line hard limit
✅ No code duplication
✅ Logical organization maintained

**Costs:**
❌ Non-standard Fortran (requires preprocessor)
❌ Poor IDE support (no syntax highlighting, go-to-definition breaks)
❌ Implicit module state sharing (no explicit interfaces)
❌ Harder to test procedures in isolation
❌ Confusing for newcomers (where is the code actually defined?)

## Migration Options Analysis

### Option 1: Fortran Submodules (Recommended for Category C)

**Applicable to:** Category C (Module Procedure Decomposition)
- `input_validation`
- `standardizer_types`
- `ast_monomorphization`

**Benefits:**
- Standard Fortran 2008 feature
- Full IDE support
- Explicit interfaces in parent module
- Independent compilation units
- Testable in isolation

**Costs:**
- Requires interface blocks in parent (adds ~50-100 lines per module)
- More files to manage
- Slightly more verbose

**Example Migration:**
```fortran
! Before: input_validation.f90 (40 lines + includes)
module input_validation
    implicit none
    private
    public :: validate_basic_syntax
    ! ... more exports
contains
    include 'input_validation_part1.inc'  ! 894 lines
    include 'input_validation_part2.inc'  ! 267 lines
end module

! After: input_validation.f90 (~150 lines with interfaces)
module input_validation
    implicit none
    private
    public :: validate_basic_syntax
    ! ... more exports

    interface
        module subroutine validate_basic_syntax(source, tokens, error_msg)
            character(len=*), intent(in) :: source
            type(token_t), intent(in) :: tokens(:)
            character(len=:), allocatable, intent(out) :: error_msg
        end subroutine
        ! ... more interfaces
    end interface
end module

! New file: input_validation_syntax_checks.f90 (~450 lines)
submodule(input_validation) input_validation_syntax_checks
contains
    module procedure validate_basic_syntax
        ! implementation
    end procedure
    ! ... related procedures
end submodule

! New file: input_validation_helpers.f90 (~450 lines)
submodule(input_validation) input_validation_helpers
contains
    module procedure format_enhanced_error
        ! implementation
    end procedure
    ! ... helper procedures
end submodule
```

### Option 2: Helper Modules (Alternative for Category C)

Create separate utility modules with explicit exports.

**Benefits:**
- Even clearer separation of concerns
- Can use helper modules from multiple places
- Standard Fortran 95 (wider compiler support)

**Costs:**
- More complex dependency graph
- Public interface required (can't be module-private)
- Circular dependency risk

**Not recommended** - submodules are cleaner for this use case.

### Option 3: Keep .inc for Categories A, B, D

**Recommendation:** Keep .inc files for large switch statements.

**Rationale:**
- These are essentially single procedures split for readability
- Extracting to submodules would require:
  - Passing all parent module state as arguments (many parameters)
  - Or creating complex state objects
  - Still end up with one giant procedure, just in a different file
- The .inc pattern is actually clearer here - it's obviously one logical unit

**Examples to keep as .inc:**
- `codegen_expressions_part*.inc` - one giant select case on expression types
- `parser_dispatcher_part*.inc` - one giant parsing dispatch function
- `ast_factory_control_part*.inc` - related factory functions with shared setup

### Option 4: Extract Common Patterns (Long-term)

For categories A & B, consider extracting common patterns:
- Visitor pattern for AST traversal (instead of manual dispatch)
- Table-driven parsers (instead of giant switch)

This is a much larger refactoring beyond scope of .inc investigation.

## Recommended Migration Plan

### Phase 1: Immediate (This PR)
- ✅ Remove 5 orphaned .inc files (2,218 lines dead code)
- ✅ Document analysis and recommendations

### Phase 2: High-Value Submodule Migration (P1)
Migrate Category C files to submodules (best ROI):

1. `input_validation` (1,161 lines → ~150 line parent + 2 submodules)
   - Estimated: 4-6 hours (many procedures, complex dependencies)

2. `standardizer_types` (1,028 lines → ~100 line parent + 2 submodules)
   - Estimated: 3-4 hours

3. `ast_monomorphization` (2,565 lines → ~200 line parent + 3 submodules)
   - Estimated: 6-8 hours (largest, most complex)

**Total Phase 2 estimate:** 13-18 hours work, removes 4,754 lines from .inc pattern

### Phase 3: Keep as .inc (Documented)
Document in CLAUDE.md when .inc is acceptable:

```markdown
## When to use .inc files

.inc files are ACCEPTABLE in these specific cases:

1. **Large switch/case statements** that must remain as single procedures
   - Parser dispatch logic (parser_dispatcher.f90)
   - Codegen type switches (codegen_expressions.f90)

2. **Requirements:**
   - Must be under 1000 lines per .inc file
   - Must have clear split points (not arbitrary line counts)
   - Parent module must be <100 lines excluding .inc content

3. **NEVER use .inc for:**
   - New code (use submodules instead)
   - Code that can be independent procedures
   - Splitting unrelated functionality
```

### Phase 4: Long-term Refactoring (P3)
Consider architectural changes to eliminate switch statements:
- Visitor pattern for AST traversal
- Table-driven parsing

## Verification

All migrations must maintain 100% test pass rate:
- Run full test suite after each file migration
- Use `git bisect` if regressions found
- Compare output files byte-for-byte where possible

## Conclusion

**.inc files served their purpose** - keeping files under 1000 lines while the codebase grew.

**Now we can do better:**
- Submodules for procedure decomposition (Categories C, D, E)
- Keep .inc for true single-procedure splits (Categories A, B)
- Document when each is appropriate

**This PR delivers immediate value:**
- Removes 2,218 lines of dead code
- Provides roadmap for systematic improvement
- No regressions (all tests still pass)

## Related Issues
- Issue #2365: Investigate and refactor .inc file usage (29 files)
- Commit 0b86ac3e: Created orphaned .inc files that were never included
