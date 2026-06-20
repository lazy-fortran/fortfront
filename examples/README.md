# Examples

## Purpose

This directory contains canonical example source files for both lazy Fortran (`.lf`) and standard Fortran (`.f90`). Examples serve a dual purpose:
1. **Documentation**: Demonstrate features, syntax, and capabilities
2. **Test inputs**: Referenced by end-to-end tests (zero-duplication policy)

**CRITICAL**: Examples are the ONLY source of truth for full program code. End-to-end tests MUST reference these files, not duplicate them inline.

## Directory Structure

- `f90/` - Standard Fortran examples (round-trip validation)
- `lf/` - Lazy Fortran examples (transformation testing)
- `hello/` - Simple hello world examples

## File Naming Convention

- **Feature-based**: `generic_functions.lf`, `array_syntax.lf`, `module_procedures.f90`
- **Issue-based**: `issue_NNNN_description.lf` (keep issue number for traceability)
- **NOT test-based**: Avoid names like `test_*.lf` (these are examples, not tests)

## Key Concepts

**Lazy Fortran Examples**
- Demonstrate type inference capabilities
- Show minimal syntax → full standard Fortran transformation
- Examples of: function inference, array inference, intent inference
- Used by transformation pipeline tests

**Standard Fortran Examples**
- Demonstrate round-trip validation (parse → emit → parse)
- Show standard Fortran constructs fortfront can handle
- Examples of: modules, derived types, interfaces, legacy features
- Used by parser correctness tests

**Issue-Specific Examples**
- Document resolved issues with working code
- Serve as regression tests
- Keep issue number for traceability: `issue_1234_array_bounds.lf`
- Example: `examples/lf/issue_2563_dot_notation_member_access.lf` (LFortran `.` member access)
- Referenced by `test_issue_1234_*.f90` tests

**Example Quality Standards**
- **Standalone**: Each example should be complete and runnable
- **Focused**: Demonstrate one primary feature clearly
- **Documented**: Comments explain what's being demonstrated
- **Realistic**: Real-world patterns, not contrived cases
- **Concise**: As short as possible while demonstrating the feature

**Zero-Duplication Enforcement**
- For complete policy details, see [CLAUDE.md Examples & Tests Organization](../CLAUDE.md#examples--tests-organization)
- CI checks test files for inline code violations (see `make check-duplication`)
- End-to-end tests MUST use `read_example()` to load these files
- Unit tests use inline code (encouraged), end-to-end tests reference examples/ (required)

## Testing Examples

Run the test suite on all examples:
```bash
./test_examples.sh
```

Test a single example:
```bash
fpm run fortfront -- < examples/lf/function_test.lf > output.f90
gfortran output.f90 -o test
./test
```

## Example Organization

### Lazy Fortran Examples (`lf/`)
- `function_*.lf` - Function type inference examples
- `array_*.lf` - Array type inference examples
- `intent_*.lf` - Intent inference examples
- `generic_*.lf` - Generic procedure examples
- `issue_*.lf` - Issue-specific regression examples

### Standard Fortran Examples (`f90/`)
- `module_*.f90` - Module examples
- `interface_*.f90` - Interface block examples
- `derived_type_*.f90` - Derived type examples
- `legacy_*.f90` - Legacy Fortran features
- `issue_*.f90` - Issue-specific standard Fortran examples
- Argument type validation (strict mode): `issue_2593_call_argument_type_mismatch.f90`
- Implied-DO index locality: `issue_2819_implied_do_index_locality_valid.f90` (valid),
  `issue_2819_implied_do_index_out_of_scope.f90` and
  `issue_2819_implied_do_index_shadow.f90` (rejected)
- Strict arg checker scope resolution: `issue_2644_strict_arg_checker_scope_lookup.f90`
- Regression coverage includes `duplicate_var_decl_regression.f90` for preventing
  auto-generated duplicate local declarations
- Program scaffold preservation: `program_scaffold_preserve.f90`,
  `program_scaffold_special_name.f90`

## Adding New Examples

When adding new examples:
1. Place in appropriate subdirectory (`f90/` or `lf/`)
2. Use descriptive name: `feature_description.lf` or `issue_NNNN_description.lf`
3. Add comment header explaining what's demonstrated
4. Test: `fpm run fortfront -- < examples/lf/filename.lf > output.f90`
5. Verify: `gfortran output.f90 && ./a.out`
6. Reference from test: `call read_example('examples/lf/filename.lf', source)`

## Usage in Tests

```fortran
! End-to-end test using example
program test_function_inference
    use transformation_api, only: transform_lazy_fortran_string
    character(len=:), allocatable :: source, output

    ! Load canonical example
    call read_example('examples/lf/function_integer_inference.lf', source)

    ! Transform
    call transform_lazy_fortran_string(source, output, errors)

    ! Validate
    call assert_no_errors(errors)
end program
```

## Dependencies

None - examples are standalone source files.
