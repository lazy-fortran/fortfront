# Fortfront Examples

This directory contains curated examples of lazy Fortran (.lf) files that demonstrate the capabilities of the fortfront compiler.

## Current Status
As of September 2025 the Pratt-based expression parser drives fortfront.
The Pratt AST output matches the precedence regression suite, but the
CLI still emits duplicate `program` blocks for those samples, so
`gfortran -fsyntax-only` fails. The remaining known gaps align with the
examples marked as expected failures in `tests/test_lazy_fortran_examples.py`
(issues #1234–#1243).

## Testing Examples
Run the test suite on all examples:
```bash
./test_examples.sh
```

## Individual Testing
Test a single example:
```bash
fpm run fortfront -- < examples/function_test.lf > output.f90
```

## Examples Description

### Working Examples
These examples are validated end-to-end:

- `function_test.lf` - Basic function definitions and calls
- `test_sem_intrinsic.lf` - Semantic intrinsic handling
- `test_sem_scope.lf` - Variable scoping validation
- `test_std_do.lf` - Do-loop generation

### Expression Regression Suite
- `test_comparison_associativity.lf`, `test_comprehensive_precedence.lf`, and
  `test_unary_precedence.lf` exercise the Pratt parser. They currently fail the
  example integration run because the CLI writes duplicate `program` stubs
  (tracking issue #1240).

### Example Categories

#### Functions
- `function_test.lf` - Function definition and calling

#### Arrays
- `test_sem_arr.lf` - Semantic array analysis

#### String Operations
- `test_std_concat.lf` - String concatenation

#### Control Flow
- `test_std_do.lf` - Do loops

#### Type System
- `test_sem_intrinsic.lf` - Intrinsic function handling
- `test_sem_scope.lf` - Variable scoping

## Contributing
When adding new examples:
1. Test with `fpm run fortfront -- < your_example.lf`
2. Verify the generated Fortran compiles with gfortran
3. Add documentation to this README
