# Type Safety Guide

## Type Validation

All type assignments go through `create_validated_type()` for validation:

```bash
fortfront examples/lf/docs_type_validation_calculate.lf
```
Output:
```fortran
program main
    implicit none
    real :: val
    val = calculate(1.0, 2.0)
contains

real function calculate(a, b) result(res)
    implicit none
    real, intent(in) :: a
    real, intent(in) :: b
    res = a + b
end function calculate
end program main
```

## Character Type Safety

String concatenation lengths computed accurately:

```bash
fortfront examples/lf/docs_string_concatenation.lf
```
Output: `character(len=11) :: message`

## Mixed Type Operations

Integer + real operations handled safely:

```bash
fortfront examples/lf/docs_mixed_type_operations.lf
```
Output: `integer, intent(in) :: i` and `real, intent(in) :: x`, with `real` return type

## Validation Contexts

All type validation includes context for error reporting:
- `binary-op-type-validation`
- `assignment-target-validation`
- `function-call-argument-validation`

## Error Handling

- **Type mismatches**: Caught during transformation
- **Invalid indices**: Creates validated type variables
- **Missing nodes**: Uses safe defaults with context
- **Error conditions**: Handles gracefully without crashes
