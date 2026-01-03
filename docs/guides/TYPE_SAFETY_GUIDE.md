# Type Safety Guide

## Type Validation

All type assignments go through `create_validated_type()` for validation:

```bash
fortfront examples/lf/docs_type_validation_calculate.lf
```
Output: `examples/f90/docs_type_validation_calculate_out.f90` (see `real function calculate(a, b) result(res)`)

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
Output: `examples/f90/docs_mixed_type_operations_out.f90` (see `integer, intent(in) :: i` and `real, intent(in) :: x`)

## Unsigned Integers

Fortfront tracks signedness for integer operations. Mixing signed and unsigned
integers implicitly is forbidden; use explicit conversion intrinsics.

Note: `uint` and `wrap_*` are fortfront intrinsics, not standard Fortran. Many
compilers will reject output that uses them (see `examples/expected_failures.txt`).

```bash
fortfront examples/lf/docs_unsigned_integers.lf
```
Output: `examples/f90/docs_unsigned_integers_out.f90` (see `u = uint(i)`)

To emit the nonstandard unsigned attribute in Fortran, use `integer, unsigned`
in standard Fortran input. See `examples/f90/issue_2591_unsigned_declarations.f90`
(note: many compilers reject this extension; see `examples/expected_failures.txt`).

### Conversion intrinsics

- `uint(x)` converts a signed integer to an unsigned integer.
- `int(x)` converts an unsigned integer back to a signed integer.

### Wraparound intrinsics

For wraparound arithmetic on unsigned integers, use:
`wrap_add(a, b)`, `wrap_sub(a, b)`, `wrap_mul(a, b)`.

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
