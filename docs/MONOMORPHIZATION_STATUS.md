# Monomorphization Implementation Status (Issue #1863)

## Current Status

**Phase 1 implementation is IN PROGRESS.** Infrastructure modules have been created but the full integration into the type inference and codegen pipeline is not yet complete.

## Problem Statement

Currently, fortfront infers types from the first call site encountered and uses that type for all subsequent calls. This causes incorrect behavior when a function is called with different argument types:

```fortran
! Input (lazy fortran)
function add(a, b)
    add = a + b
end function

x = add(5, 3)        ! integer call
y = add(2.5d0, 1.5d0)  ! real call - ERROR: y inferred as integer
```

**Current output:** Both `x` and `y` are `integer`, second call produces incorrect result.

**Expected output:** Generate two specializations (`add__i32_i32` and `add__r64_r64`) with a generic interface.

## Infrastructure Created

### 1. Name Mangling Module (`src/codegen/codegen_name_mangling.f90`)

Provides deterministic name mangling for procedure specializations:

- `mangle_procedure_name(base_name, signature)` - Generate mangled names
- `type_signature_to_string(signature)` - Human-readable signature format
- Format: `<name>__<kind1>_<kind2>_...`
- Examples: `add__i32_i32`, `add__r64_r64`, `matmul__r64rank2_r64rank2`

**Status:** ✅ Implemented and compiles

### 2. Type Signature Tracking Module (`src/analysis/call_graph_signatures_mod.f90`)

Tracks unique type signatures for each procedure across all call sites:

- `type_signature_t` - Stores parameter kinds, return kind, and location info
- `signatures_map_t` - Maps procedure names to their unique signatures
- `add_signature()` - Add a call site signature (deduplicates automatically)
- `get_unique_signatures()` - Retrieve all unique signatures for a procedure

**Status:** ✅ Implemented and compiles

### 3. Integration Test (`test/integration/monomorphization/test_monomorphization_simple.f90`)

Demonstrates expected behavior and current limitation.

**Status:** ✅ Runs and shows current behavior (single type inference)

## Remaining Work

### High Priority: Complete Phase 1

1. **Integrate signature collection into semantic analysis**
   - Modify `src/semantic/analyzers/` to collect call site type information
   - Store signatures in `semantic_context_t` or pass to codegen
   - Ensure all call sites are analyzed before codegen runs

2. **Enhance codegen to generate multiple variants**
   - Modify `src/codegen/codegen_declarations_procedures.f90`:
     - `generate_code_function_def()` - Generate all specializations
     - Clone function body AST for each signature
     - Apply mangled names to each variant
   - Add support for subroutines (not just functions)

3. **Generate generic interface blocks**
   - Create `generate_interface_block()` in codegen
   - Emit `interface <name>` with `module procedure` list
   - Only generate when multiple specializations exist
   - Keep single-specialization functions simple (no interface overhead)

4. **Wrap in module when needed**
   - Detect when monomorphization creates multiple procedures
   - Generate `module auto_<name>` to contain interface + specializations
   - Update main program to `use` the generated module

5. **Add comprehensive tests**
   - Two-type case (integer + real)
   - Three-type case (integer + real + complex)
   - Arrays with different ranks
   - Subroutines (not just functions)
   - Single-type case (ensure no interface overhead)

### Design Decisions Needed

1. **Where to store signature map?**
   - Option A: Add to `semantic_context_t`
   - Option B: Pass as separate parameter through pipeline
   - Option C: Global module-level variable (not recommended)

2. **When to collect signatures?**
   - During type inference passes (may require additional pass)
   - After semantic analysis completes
   - As part of call graph building (would require call graph enhancement)

3. **How to handle result variable names?**
   - Use `result(res)` clause for all mangled functions
   - Or use function name as result variable (current behavior)

4. **Maximum specializations per function?**
   - Default cap at 10 unique signatures?
   - Warn user if exceeded?
   - Make configurable via flag?

## Technical Challenges

### Type Inference Order

Currently fortfront does multiple passes until types converge. Monomorphization requires:
- All call sites analyzed before finalizing function types
- Handling interdependent functions (A calls B, B calls A with different types)
- Avoiding infinite specialization loops

### AST Cloning

Need to clone function body AST for each specialization:
- Arena-based allocation makes cloning non-trivial
- Must preserve node relationships (indices remain valid)
- Consider: Clone into same arena vs separate arenas

### Module vs Program Output

Current behavior:
- Bare functions → wrapped in `program main` with `contains`
- Need to switch to module when monomorphization occurs

Proposed:
- Single specialization → keep current behavior (program with contains)
- Multiple specializations → generate module + program that uses it

### Character Type Handling

Character types with different lengths need special handling:
- `character(len=10)` vs `character(len=20)` are different types
- Current kind-based mangling doesn't capture length
- May need enhanced signature representation

## Testing Strategy

### Unit Tests (per module)
- `test_name_mangling.f90` - Uniqueness, determinism
- `test_signature_tracking.f90` - Deduplication, retrieval
- `test_signature_extraction.f90` - Extract types from call sites

### Integration Tests (full pipeline)
- `test_monomorphization_simple.f90` - ✅ Created (currently fails as expected)
- `test_monomorphization_arrays.f90` - Different array ranks
- `test_monomorphization_mixed.f90` - Scalars + arrays
- `test_single_type_simple.f90` - No interface when only one type used

### Regression Tests
- All existing examples in `examples/lf/` must continue to work
- Verify single-type functions don't change output format

## Files Modified/Created

### Created
- `src/codegen/codegen_name_mangling.f90` - Name mangling
- `src/analysis/call_graph_signatures_mod.f90` - Signature tracking
- `test/integration/monomorphization/test_monomorphization_simple.f90` - Test
- `examples/lf/monomorphization_simple.lf` - Example input
- `docs/MONOMORPHIZATION_STATUS.md` - This file

### To Be Modified
- `src/semantic/analyzers/*.f90` - Add signature collection
- `src/semantic/types/semantic_context_types.f90` - Add signatures field
- `src/codegen/codegen_declarations_procedures.f90` - Generate variants
- `src/codegen/codegen_declarations_programs.f90` - Interface generation
- `src/frontend_transformation.f90` - Pipeline orchestration
- `fpm.toml` - Ensure new test discovered

## Example Output (Goal)

```fortran
! Input: monomorphization_simple.lf
function add(a, b)
    add = a + b
end function

x = add(5, 3)
y = add(2.5d0, 1.5d0)
```

```fortran
! Expected output
module auto_add
    implicit none
    interface add
        module procedure add__i32_i32, add__r64_r64
    end interface add
contains
    integer function add__i32_i32(a, b)
        integer, intent(in) :: a, b
        add__i32_i32 = a + b
    end function add__i32_i32

    real(8) function add__r64_r64(a, b)
        real(8), intent(in) :: a, b
        add__r64_r64 = a + b
    end function add__r64_r64
end module auto_add

program main
    use auto_add
    implicit none
    integer :: x
    real(8) :: y
    x = add(5, 3)
    y = add(2.5d0, 1.5d0)
end program main
```

## References

- Issue #1863: Implement single-file monomorphization (Phase 1)
- `docs/MONOMORPHIZATION.md`: Detailed design document
- `docs/LIBRARY_USAGE.md`: API documentation
- `docs/SEMANTIC_PIPELINE_ARCHITECTURE.md`: Type inference architecture

## Next Steps

1. Decide on signature map storage location (semantic context vs separate)
2. Implement signature collection in semantic analyzer
3. Implement variant generation in codegen
4. Implement interface block generation
5. Add module wrapping logic
6. Complete test suite
7. Run regression tests
8. Update documentation

## Success Criteria (from issue #1863)

- [x] Infrastructure modules created
- [ ] Multiple type uses in single file generate correct code
- [ ] Generated Fortran compiles with gfortran
- [ ] Generated program runs and produces correct output
- [ ] Single-type functions stay simple (no interface)
- [ ] All existing examples continue to work
- [ ] Test coverage ≥90% for new code
- [ ] Documentation complete
