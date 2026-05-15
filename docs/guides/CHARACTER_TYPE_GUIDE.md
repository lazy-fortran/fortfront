# Character Type Handling

## Automatic Length Inference

fortfront calculates character lengths from string literals:

```bash
fortfront examples/lf/docs_character_length_inference.lf
```
Output: `examples/f90/docs_character_length_inference_out.f90` (see `character(len=5) :: name`)

## String Concatenation

Combined lengths are calculated automatically:

```bash
fortfront examples/lf/docs_string_concatenation.lf
```
Output: `examples/f90/docs_string_concatenation_out.f90` (see `character(len=11) :: message`)

## Reassignment With Different Lengths

When assigned strings of different lengths, uses a fixed-length character sized
to the longest value:

```bash
fortfront examples/lf/docs_variable_length_strings.lf
```
Output: `examples/f90/docs_variable_length_strings_out.f90` (see `character(len=5) :: message`)

Same-length assignments use fixed-length:
```bash
fortfront examples/lf/docs_fixed_length_reassignment.lf
```
Output: `examples/f90/docs_fixed_length_reassignment_out.f90` (see `character(len=3) :: code`)

## Character Arrays

Arrays use maximum element length with padding:

```bash
fortfront examples/lf/docs_character_arrays.lf
```
Output: `examples/f90/docs_character_arrays_out.f90` (see `character(len=7) :: names(3)`)

## Limitation

Function parameters used in character operations default to `real(dp)` instead of being inferred from context. This is a known limitation.
