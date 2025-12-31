# Type Safety Guide

## Type Validation

All type assignments go through `create_validated_type()` for validation:

```fortran
! Input
function calculate(a, b) result(sum)
    sum = a + b
end function

! Output (types inferred and validated)
function calculate(a, b) result(sum)
    implicit none
    real(8), intent(in) :: a, b
    real(8) :: sum
    sum = a + b
end function calculate
```

## Character Type Safety

String concatenation lengths computed accurately:

```bash
echo 'message = "hello" // " world"' | fortfront
```
Output: `character(len=11) :: message`

## Mixed Type Operations

Integer + real operations handled safely:

```fortran
function mixed_calc(i, x) result(y)
    y = i + x
end function
```
Output: `integer, intent(in) :: i` and `real(8), intent(in) :: x`, with `real(8) :: y`

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
