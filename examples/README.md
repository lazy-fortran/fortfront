# Fortfront Examples

This directory contains curated examples of lazy Fortran (.lf) files that demonstrate the capabilities of the fortfront compiler.

## Current Status
⚠️ **Note**: As of September 2025, fortfront has significant issues:
- 65.8% of test files fail with "Unrecognized token in expression" errors
- Performance is very poor (~1.7 seconds per file)
- Many basic constructs are not working correctly

See GitHub issues:
- [#1229](https://github.com/lazy-fortran/fortfront/issues/1229) - Parser errors
- [#1230](https://github.com/lazy-fortran/fortfront/issues/1230) - Performance issues

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
These examples were verified to work correctly:

- `function_test.lf` - Basic function definitions and calls
- `test_209_*.lf` - Various test cases for issue #209
- `test_comparison_associativity.lf` - Operator precedence testing
- `test_comprehensive_precedence.lf` - Expression precedence
- `test_sem_*.lf` - Semantic analysis tests
- `test_std_*.lf` - Standard library features

### Example Categories

#### Functions
- `function_test.lf` - Function definition and calling

#### Arrays  
- `test_std_arr.lf` - Array operations
- `test_sem_arr.lf` - Semantic array analysis

#### String Operations
- `test_std_concat.lf` - String concatenation

#### Control Flow
- `test_std_do.lf` - Do loops
- `test_std_slice.lf` - Array slicing

#### Type System
- `test_sem_intrinsic.lf` - Intrinsic function handling
- `test_sem_scope.lf` - Variable scoping

## Contributing
When adding new examples:
1. Test with `fpm run fortfront -- < your_example.lf`
2. Verify the generated Fortran compiles with gfortran
3. Add documentation to this README